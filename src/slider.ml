open Core_kernel
open Import
open Bonsai.Infix
open Components
module Attr = Attributes

type length =
  | Dynamic of int
  | Static of int

let length_val = function
  | Dynamic v -> v
  | Static v -> v


let length_to_styles vertical reverse thickness = function
  | Static i ->
    Style.[ width (if vertical then thickness else i); height (if vertical then i else thickness) ]
  | Dynamic i ->
    if vertical
    then
      Style.
        [ width thickness
        ; max_height i
        ; flex_direction (if reverse then `ColumnReverse else `Column)
        ]
    else
      Style.
        [ height thickness; max_width i; flex_direction (if reverse then `RowReverse else `Row) ]


type props =
  { on_value_changed : float -> Event.t
  ; vertical : bool
  ; reverse : bool
  ; min_value : float
  ; max_value : float
  ; init_value : float
  ; slider_length : length
  ; track_thickness : int
  ; track_color : Color.t
  ; thumb : Draggable.props
  }

let props
    ?(on_value_changed = fun _ -> Event.no_op)
    ?(vertical = true)
    ?(reverse = false)
    ?(min_value = 0.)
    ?(max_value = 1.)
    ?(init_value = 0.)
    ?(slider_length = Static 100)
    ?(thumb_length = Static 15)
    ?(thumb_thickness = 15)
    ?(track_thickness = 15)
    ?(track_color = Colors.dark_gray)
    ?(thumb_color = Colors.gray)
    ()
  =
  let thumb =
    Draggable.props
      ~snap_back:false
      ~freedom:(if vertical then Y else X)
      Style.(
        background_color thumb_color
        :: length_to_styles vertical reverse thumb_thickness thumb_length) in
  { on_value_changed
  ; vertical
  ; reverse
  ; min_value
  ; max_value
  ; init_value
  ; slider_length
  ; track_thickness
  ; track_color
  ; thumb
  }


module T = struct
  module Model = struct
    type t =
      { mutable initialized : bool
      ; bounding_box : (BoundingBox2d.t[@sexp.opaque]) option
      }
    [@@deriving equal, sexp]

    let default = { initialized = false; bounding_box = None }
  end

  module Action = struct
    type t = SetBoundingBox of (BoundingBox2d.t[@sexp.opaque]) [@@deriving sexp_of]
  end

  module Input = struct
    type t =
      BoundingBox2d.t option
      * (BoundingBox2d.t -> Event.t)
      * (float -> float -> Event.t)
      * Element.t
      * props
  end

  open Action

  module Result = struct
    type t = float * Element.t
  end

  let name = "Slider"

  let travel vertical slider_bb bar_bb =
    let s_l, s_t, s_r, s_b = BoundingBox2d.get_bounds slider_bb in
    let b_l, b_t, b_r, b_b = BoundingBox2d.get_bounds bar_bb in
    if vertical
    then (s_t -. b_t) /. (s_t -. s_b -. b_t +. b_b +. 0.000001)
    else (s_l -. b_l) /. (s_l -. s_r -. b_l +. b_r +. 0.000001)


  let value_to_travel v min max = (v -. min) /. (max -. min)
  let travel_to_value trav min max = (trav *. (max -. min)) +. min

  let apply_shift vertical slider_bb bar_bb shift trav =
    let s_l, s_t, s_r, s_b = BoundingBox2d.get_bounds slider_bb in
    let b_l, b_t, b_r, b_b = BoundingBox2d.get_bounds bar_bb in
    if vertical
    then shift 0. (trav *. (s_b -. s_t -. b_b +. b_t))
    else shift (trav *. (s_r -. s_l -. b_r +. b_l)) 0.


  let compute ~inject ((bar_bb, set_bar_bb, shift_bar, bar, props) : Input.t) (model : Model.t) =
    let () =
      if not model.initialized
      then (
        match model.bounding_box, bar_bb with
        | Some bb, Some bar_bb ->
          value_to_travel props.init_value props.min_value props.max_value
          |> apply_shift props.vertical bb bar_bb shift_bar
          |> Event.Expert.handle;
          model.initialized <- true
        | _ -> () ) in

    let handle_bounding_box_change bb = Event.Many [ inject (SetBoundingBox bb); set_bar_bb bb ] in
    let value =
      Option.both model.bounding_box bar_bb
      |> Option.value_map ~default:props.init_value ~f:(fun (bb, bar_bb) ->
             let v = travel props.vertical bb bar_bb in
             travel_to_value v props.min_value props.max_value) in
    let styles =
      let open Style in
      background_color props.track_color
      :: (if props.vertical then align_items `Center else justify_content `Center)
      :: length_to_styles props.vertical props.reverse props.track_thickness props.slider_length
    in
    let element =
      box Attr.[ on_bounding_box_changed handle_bounding_box_change; style styles ] [ bar ] in
    value, element


  let apply_action ~inject:_ ~schedule_event:_ _ (model : Model.t) = function
    | SetBoundingBox bb -> { model with bounding_box = Some bb }
end

let component =
  Bonsai.pure ~f:(fun (props : props) -> text [] "", props.thumb)
  >>> Draggable.component
  |> Bonsai.Arrow.extend_first
  |> Bonsai.map ~f:(fun ((bb, set_bounds, shift, draggable), props) ->
         bb, set_bounds, shift, draggable, props)
  >>> Bonsai.of_module (module T) ~default_model:T.Model.default


let with_thumb = Bonsai.of_module (module T) ~default_model:T.Model.default
