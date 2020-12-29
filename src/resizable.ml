open Core_kernel
open Import
open Bonsai.Infix
open Components
module Attr = Attributes

type resize =
  [ `Scale of float option * float option
  | `Set of int option * int option
  ]
[@@deriving sexp_of]

type props =
  { styles : Style.t list
  ; attributes : Attr.t list
  ; max_width : int option
  ; max_height : int option
  }

let props ?(attributes = []) ?max_width ?max_height styles =
  { styles; attributes; max_width; max_height }


module T = struct
  module Model = struct
    type t =
      { set_width : int option
      ; set_height : int option
      ; start_width : int option
      ; start_height : int option
      }
    [@@deriving equal, sexp]

    let default = { set_width = None; set_height = None; start_width = None; start_height = None }
  end

  module Action = struct
    type t =
      | Resize of resize
      | OriginalDimensions of (int * int)
    [@@deriving sexp_of]
  end

  module Input = struct
    type t = Element.t * props
  end

  open Action

  module Result = struct
    type t = (resize -> Event.t) * Element.t
  end

  let name = "Resizable"

  let compute ~inject ((child, props) : Input.t) (model : Model.t) =
    let resize r = inject (Resize r) in
    let handle_dimensions_changed ({ width; height } : Node_events.Dimensions_changed.t) =
      inject (OriginalDimensions (width, height)) in
    let changed =
      List.filter_opt
        Style.[ Option.map ~f:width model.set_width; Option.map ~f:height model.set_height ] in
    let element =
      box
        Attr.(
          style (props.styles @ changed)
          :: on_dimensions_changed handle_dimensions_changed
          :: props.attributes)
        [ child ] in
    resize, element


  let apply_action ~inject:_ ~schedule_event:_ _ (model : Model.t) = function
    | OriginalDimensions (w, h) ->
      if Option.is_none model.start_width
      then { model with start_width = Some w; start_height = Some h }
      else model
    | Resize (`Set (w_opt, h_opt)) ->
      ( match w_opt, h_opt with
      | (Some _ as w), (Some _ as h) -> { model with set_width = w; set_height = h }
      | (Some _ as w), None -> { model with set_width = w }
      | None, (Some _ as h) -> { model with set_height = h }
      | None, None -> model )
    | Resize (`Scale (w_mul_opt, h_mul_opt)) ->
      ( match model.start_width, model.start_height with
      | Some w0, Some h0 ->
        let w0 = Float.of_int w0 in
        let h0 = Float.of_int h0 in
        ( match w_mul_opt, h_mul_opt with
        | Some w_mul, Some h_mul ->
          { model with
            set_width = Some (Int.of_float (w_mul *. w0))
          ; set_height = Some (Int.of_float (h_mul *. h0))
          }
        | Some w_mul, None -> { model with set_width = Some (Int.of_float (w_mul *. w0)) }
        | None, Some h_mul -> { model with set_height = Some (Int.of_float (h_mul *. h0)) }
        | None, None -> model )
      | _ -> model )
end

let component = Bonsai.of_module (module T) ~default_model:T.Model.default
