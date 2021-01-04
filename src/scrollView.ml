open Core_kernel
open Import
open Bonsai.Infix
open Components
module Attr = Attributes

type props =
  { speed : float
  ; styles : Style.t list
  ; attributes : Attr.t list
  ; min_thumb_size : int
  ; x_slider : Slider.props
  ; y_slider : Slider.props
  }

let props
    ?(speed = 25.)
    ?(attributes = [])
    ?track_color
    ?thumb_color
    ?(min_thumb_size = 20)
    ?(x_reverse = false)
    ?(y_reverse = false)
    styles
  =
  let common = Slider.props ?track_color ?thumb_color in
  let x_length =
    Slider.Dynamic
      ( List.find_map styles ~f:(function
          | `MaxWidth w -> Some w
          | _ -> None)
      |> Option.value ~default:Int.max_value ) in
  let y_length =
    Slider.Dynamic
      ( List.find_map styles ~f:(function
          | `MaxHeight h -> Some h
          | _ -> None)
      |> Option.value ~default:Int.max_value ) in
  let x_slider =
    common
      ~vertical:false
      ~reverse:x_reverse
      ~slider_length:x_length
      ~thumb_length:x_length
      ~min_value:(if x_reverse then -1. else 0.)
      ~max_value:(if x_reverse then 0. else 1.)
      () in
  let y_slider =
    common
      ~vertical:true
      ~reverse:y_reverse
      ~slider_length:y_length
      ~thumb_length:y_length
      ~min_value:(if y_reverse then -1. else 0.)
      ~max_value:(if y_reverse then 0. else 1.)
      () in
  { speed; styles; attributes; min_thumb_size; x_slider; y_slider }


let is_mac =
  Revery.Environment.(
    match os with
    | Mac -> true
    | _ -> false)


let horizonal_scroll_multiplier = if is_mac then -1. else 1.

(* Default is column flex. *)
let is_columnar : Style.t list -> bool =
  List.for_all ~f:(function
    | `FlexDirection (d : Style.flex_direction) ->
      ( match d with
      | Column | ColumnReverse -> true
      | _ -> false )
    | _ -> true)


module T = struct
  module Model = struct
    type t =
      { x_pos : float
      ; y_pos : float
      ; scroll_width : float
      ; scroll_height : float
      ; view_node : (UI.node[@sexp.opaque] [@equal.ignore]) option
      }
    [@@deriving equal, sexp]

    let default =
      { x_pos = 0.; y_pos = 0.; scroll_width = 0.; scroll_height = 0.; view_node = None }
  end

  module Action = struct
    type t =
      | Scroll of float * float
      | Scrollable of float * float
      | SetViewNode of (UI.node[@sexp.opaque])
    [@@deriving sexp_of]
  end

  module Input = struct
    type control = float * float

    type slider =
      { element : Element.t
      ; resize : Resizable.resize -> Event.t
      ; shift : float -> float -> Event.t
      }

    type sliders =
      { x : slider
      ; y : slider
      }

    type t = control * sliders * Element.t list * props
  end

  open Action
  module Result = Element

  let name = "ScrollView"
  let excess total limit = Float.(of_int Int.(total - limit) |> clamp_exn ~min:0. ~max:max_value)

  (* Sum over the major dimension, max over the minor dimension. *)
  let scrollable columnar outer_width outer_height node =
    let inner_width, inner_height =
      let w_fun, h_fun = if columnar then Int.max, ( + ) else ( + ), Int.max in
      let f (w_acc, h_acc) child =
        let dims : Revery.UI.Dimensions.t = child#measurements () in
        w_fun w_acc dims.width, h_fun h_acc dims.height in
      List.fold ~init:(0, 0) ~f (node#getChildren ()) in
    excess inner_width outer_width, excess inner_height outer_height


  let compute ~inject ((control, sliders, children, props) : Input.t) (model : Model.t) =
    let outer_width, outer_height, (scroll_width, scroll_height) =
      match model.view_node with
      | None -> 0, 0, (0., 0.)
      | Some node ->
        let dims : Revery.UI.Dimensions.t = node#measurements () in
        dims.width, dims.height, scrollable (is_columnar props.styles) dims.width dims.height node
    in
    let () =
      (* NOTE: I'm wary of using Expert.handle, but since this only fires when there are relevant
         changes, I don't think that it should overwhelm the scheduler. *)
      if Float.(model.scroll_width <> scroll_width || model.scroll_height <> scroll_height)
      then (
        let h = Int.max props.min_thumb_size (outer_height - Int.of_float scroll_height) in
        let w = Int.max props.min_thumb_size (outer_width - Int.of_float scroll_width) in
        Event.Many
          [ inject (Scrollable (scroll_width, scroll_height))
          ; sliders.y.resize (`Set (None, Some h))
          ; sliders.x.resize (`Set (Some w, None))
          ]
        |> Event.Expert.handle ) in

    let handle_wheel ({ shiftKey; deltaY; _ } : Node_events.Mouse_wheel.t) =
      let delta = deltaY *. props.speed *. -1. in
      match Float.(abs delta > 0.), shiftKey with
      | true, false -> sliders.y.shift 0. delta
      | true, true -> sliders.x.shift (delta *. horizonal_scroll_multiplier) 0.
      | _ -> Event.no_op in

    let trans_x = fst control *. scroll_width in
    let trans_y = snd control *. scroll_height in
    let trans =
      Attr.
        [ style Style.[ transform [ TranslateX (-1. *. trans_x); TranslateY (-1. *. trans_y) ] ] ]
    in

    let view =
      box
        Attr.(
          on_mouse_wheel handle_wheel
          :: node_ref (fun n -> inject (SetViewNode n))
          :: style Style.(overflow `Hidden :: props.styles)
          :: props.attributes)
        (List.map ~f:(fun c -> box trans [ c ]) children) in

    let element =
      let inner_box =
        let styles = Style.[ flex_direction `Row; margin_bottom 2 ] in
        let elements = view :: (if Float.(scroll_height > 0.) then [ sliders.y.element ] else []) in
        box Attr.[ style styles ] elements in
      if Float.(scroll_width > 0.) then box [] [ inner_box; sliders.x.element ] else inner_box in
    box (Attr.(style props.styles) :: props.attributes) [ element ]


  let apply_action ~inject:_ ~schedule_event:_ _ (model : Model.t) = function
    | Scroll (x_pos, y_pos) -> { model with x_pos; y_pos }
    | Scrollable (w, h) -> { model with scroll_width = w; scroll_height = h }
    | SetViewNode node -> { model with view_node = Some node }
end

let slider_props ax props =
  match ax with
  | `X -> props.x_slider
  | `Y -> props.y_slider


let compose_slider ax =
  let get_props = slider_props ax in
  let thumb =
    Bonsai.Arrow.pipe
      ( Bonsai.pure ~f:(fun (children, (props : props)) ->
            let resize_props =
              Resizable.props
                ~attributes:(get_props props).thumb.attributes
                (get_props props).thumb.styles in
            text [] "", resize_props)
      >>> Resizable.component )
      ~via:(fun (_, props) (resize, thumb) -> thumb, (get_props props).thumb)
      ~into:Draggable.component
      ~finalize:(fun props (resize, thumb) (bb, set_bounds, shift, draggable) ->
        bb, set_bounds, shift, resize, draggable) in
  Bonsai.Arrow.pipe
    thumb
    ~via:(fun (children, props) (bb, set_bounds, shift, resize, draggable) ->
      bb, set_bounds, shift, draggable, get_props props)
    ~into:Slider.with_thumb
    ~finalize:(fun (children, props) (bb, set_bounds, shift, resize, draggable) (value, slider) ->
      value, shift, resize, slider)


let component =
  Bonsai.Arrow.fanout (compose_slider `X) (compose_slider `Y)
  |> Bonsai.Arrow.extend_first
  |> Bonsai.map
       ~f:(fun
            ( ((x_value, x_shift, x_resize, x_slider), (y_value, y_shift, y_resize, y_slider))
            , (children, props) )
          ->
         let sliders =
           T.Input.
             { x = { element = x_slider; shift = x_shift; resize = x_resize }
             ; y = { element = y_slider; shift = y_shift; resize = y_resize }
             } in
         (x_value, y_value), sliders, children, props)
  >>> Bonsai.of_module (module T) ~default_model:T.Model.default
