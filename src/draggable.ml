open Core_kernel
open Import
open Bonsai.Infix
open Components
module Attr = Attributes

type freedom =
  | X
  | Y
  | Free

type props =
  { styles : Style.t list
  ; attributes : Attr.t list
  ; freedom : freedom
  ; snap_back : bool
  ; on_drag : bb:BoundingBox2d.t -> x:float -> y:float -> Event.t
  ; on_drop : bb:BoundingBox2d.t -> x:float -> y:float -> Event.t
  }

let props
    ?(attributes = [])
    ?(freedom = Free)
    ?(snap_back = false)
    ?(on_drag = fun ~bb:_ ~x:_ ~y:_ -> Event.no_op)
    ?(on_drop = fun ~bb:_ ~x:_ ~y:_ -> Event.no_op)
    styles
  =
  { styles; attributes; freedom; snap_back; on_drag; on_drop }


module T = struct
  module Model = struct
    type t =
      { grabbed : bool
      ; x_trans : float
      ; y_trans : float
      ; inner_box : (BoundingBox2d.t[@sexp.opaque]) option
      ; outer_box : (BoundingBox2d.t[@sexp.opaque]) option
      ; node : (UI.node[@sexp.opaque] [@equal.ignore]) option
      }
    [@@deriving equal, sexp]

    let default =
      { grabbed = false
      ; x_trans = 0.
      ; y_trans = 0.
      ; inner_box = None
      ; outer_box = None
      ; node = None
      }
  end

  module Action = struct
    type t =
      | Grab
      | Drop
      | Drag of float * float
      | Shift of float * float
      | InnerBox of (BoundingBox2d.t[@sexp.opaque])
      | OuterBox of (BoundingBox2d.t[@sexp.opaque])
      | Set_node of (UI.node[@sexp.opaque])
    [@@deriving sexp_of]
  end

  module Input = struct
    type t = Element.t * props
  end

  open Action

  module Result = struct
    type t =
      BoundingBox2d.t option
      * (BoundingBox2d.t -> Event.t)
      * (float -> float -> Event.t)
      * Element.t
  end

  let name = "Draggable"

  let allowable_movement inner outer =
    let i_l, i_t, i_r, i_b = BoundingBox2d.get_bounds inner in
    let o_l, o_t, o_r, o_b = BoundingBox2d.get_bounds outer in
    let min_x = o_l -. i_l in
    let max_x = o_r -. i_r in
    let min_y = o_t -. i_t in
    let max_y = o_b -. i_b in
    min_x, min_y, max_x, max_y


  let boundless = Float.(-.max_value, -.max_value, max_value, max_value)

  let shift freedom bb boundary x0 y0 x1 y1 =
    let min_x, min_y, max_x, max_y =
      Option.value_map ~default:boundless ~f:(allowable_movement bb) boundary in
    match freedom with
    | Free ->
      ( Float.clamp_exn (x1 -. x0) ~min:min_x ~max:max_x
      , Float.clamp_exn (y1 -. y0) ~min:min_y ~max:max_y )
    | X -> Float.clamp_exn (x1 -. x0) ~min:min_x ~max:max_x, 0.
    | Y -> 0., Float.clamp_exn (y1 -. y0) ~min:min_y ~max:max_y


  let compute ~inject ((child, props) : Input.t) (model : Model.t) =
    let handle_mouse_up state ({ button; _ } : Node_events.Mouse_button.t) =
      let x0, y0, corner_x, corner_y, x_trans, y_trans = state in
      ( Event.Many
          ( match button with
          | BUTTON_LEFT ->
            let drop =
              Option.map model.node ~f:(fun n ->
                  props.on_drop
                    ~x:(corner_x +. x_trans)
                    ~y:(corner_y +. y_trans)
                    ~bb:(n#getBoundingBox ())) in
            inject Drop
            :: List.filter_opt
                 [ drop; (if props.snap_back then Some (inject (Drag (0., 0.))) else None) ]
          | _ -> [ Event.no_op ] )
      , None ) in
    let handle_mouse_move state ({ mouseX = x1; mouseY = y1; _ } : Node_events.Mouse_move.t) =
      let x0, y0, corner_x, corner_y, x_trans, y_trans = state in
      match model.node with
      | Some node ->
        let bb = node#getBoundingBox () in
        let i_l, i_t, i_r, i_b = BoundingBox2d.get_bounds bb in
        let o_l, o_t, o_r, o_b =
          Option.value_map ~default:boundless ~f:BoundingBox2d.get_bounds model.outer_box in
        let x =
          match props.freedom with
          | Free | X ->
            Float.clamp_exn (x1 -. x0) ~min:(o_l -. corner_x) ~max:(o_r -. corner_x +. (i_l -. i_r))
          | Y -> 0. in
        let y =
          match props.freedom with
          | Free | Y ->
            Float.clamp_exn (y1 -. y0) ~min:(o_t -. corner_y) ~max:(o_b -. corner_y +. (i_t -. i_b))
          | X -> 0. in
        ( Event.Many
            [ inject (Drag (x, y)); props.on_drag ~bb ~x:(x +. corner_x) ~y:(y +. corner_y) ]
        , Some (x0, y0, corner_x, corner_y, x, y) )
      | _ -> Event.no_op, None in
    let handle_mouse_down ({ button; mouseX; mouseY; _ } : Node_events.Mouse_button.t) =
      match button, model.node with
      | BUTTON_LEFT, Some node ->
        let inner_world = node#getWorldTransform () in
        let corner_x = Skia.Matrix.getTranslateX inner_world in
        let corner_y = Skia.Matrix.getTranslateY inner_world in
        mouse_capture
          ~on_mouse_move:handle_mouse_move
          ~on_mouse_up:handle_mouse_up
          ( mouseX -. model.x_trans
          , mouseY -. model.y_trans
          , corner_x -. model.x_trans
          , corner_y -. model.y_trans
          , model.x_trans
          , model.y_trans );
        Event.no_op
      | _ -> Event.no_op in

    let handle_bounding_box_change bb = inject (InnerBox bb) in
    let trans = Style.(transform [ TranslateX model.x_trans; TranslateY model.y_trans ]) in

    let shift_callback x y = inject (Shift (x, y)) in
    let set_bounds bb = inject (OuterBox bb) in
    let element =
      box
        Attr.(
          on_mouse_down handle_mouse_down
          :: on_bounding_box_changed handle_bounding_box_change
          :: node_ref (fun n -> inject (Set_node n))
          :: style (trans :: props.styles)
          :: props.attributes)
        [ child ] in
    model.inner_box, set_bounds, shift_callback, element


  let apply_action ~inject:_ ~schedule_event:_ ((_, props) : Input.t) (model : Model.t) = function
    | Grab -> { model with grabbed = true }
    | Drop -> { model with grabbed = false }
    | Drag (x, y) -> { model with x_trans = x; y_trans = y }
    | Shift (x, y) ->
      ( match model.inner_box with
      | None -> model
      | Some inner_box ->
        let x0, y0 =
          let l, t, r, b = BoundingBox2d.get_bounds inner_box in
          (l +. r) /. 2., (t +. b) /. 2. in
        let x_pos = x0 +. model.x_trans in
        let y_pos = y0 +. model.y_trans in
        let shift_x, shift_y =
          shift props.freedom inner_box model.outer_box x_pos y_pos (x_pos +. x) (y_pos +. y) in
        { model with x_trans = shift_x +. model.x_trans; y_trans = shift_y +. model.y_trans } )
    | InnerBox bb -> { model with inner_box = Some bb }
    | OuterBox bb -> { model with outer_box = Some bb }
    | Set_node node -> { model with node = Some node }
end

let component = Bonsai.of_module (module T) ~default_model:T.Model.default
