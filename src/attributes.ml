open Core_kernel
open Import

type custom_events =
  { on_left_click : Event.t option
  ; on_right_click : Event.t option
  ; on_any_click : Event.t option
  }
[@@deriving fields]

type native_event =
  | Ref of (UI.node -> Event.t)
  | Mouse_down of (Node_events.Mouse_button.t -> Event.t)
  | Mouse_move of (Node_events.Mouse_move.t -> Event.t)
  | Mouse_up of (Node_events.Mouse_button.t -> Event.t)
  | Mouse_wheel of (Node_events.Mouse_wheel.t -> Event.t)
  | Key_down of (Node_events.Keyboard.t -> Event.t)
  | Key_up of (Node_events.Keyboard.t -> Event.t)
  | Text_input of (Node_events.Text_input.t -> Event.t)
  | Text_edit of (Node_events.Text_edit.t -> Event.t)
  | Mouse_enter of (Node_events.Mouse_move.t -> Event.t)
  | Mouse_leave of (Node_events.Mouse_move.t -> Event.t)
  | Mouse_over of (Node_events.Mouse_move.t -> Event.t)
  | Mouse_out of (Node_events.Mouse_move.t -> Event.t)
  | Dimensions_changed of (Node_events.Dimensions_changed.t -> Event.t)
  | Blur of Event.t
  | Focus of Event.t

type custom_event =
  | Left_click of Event.t
  | Right_click of Event.t
  | Any_click of Event.t

type t =
  | Native_event_handler of native_event
  | Custom_event_handler of custom_event
  | Style of UI.Style.t
  | Tab_index of int

let sexp_of_t = sexp_of_opaque
let default_custom_events = { on_left_click = None; on_right_click = None; on_any_click = None }

type attributes =
  { mutable style : UI.Style.t
  ; mutable native_events : UI.node UI.NodeEvents.t
  ; mutable custom_events : custom_events
  ; mutable tab_index : int option
  }
[@@deriving fields]

let default_font_family =
  let font_path family =
    match (Revery.Font.Discovery.find family ~weight:FontManager.FontWeight.Normal).path with
    | "" -> None
    | path -> Some path in
  [ "Noto Sans"; "Liberation Sans"; "DejaVu Sans" ]
  |> List.find_map ~f:font_path
  |> Option.value ~default:""


let default_style = { UI.Style.defaultStyle with fontFamily = default_font_family }

let make_attributes
    ?(style = default_style)
    ?(native_events = UI.NodeEvents.make ())
    ?tab_index
    ?(custom_events = default_custom_events)
    ()
  =
  { style; native_events; tab_index; custom_events }


let update_node attributes node =
  Fields_of_attributes.Direct.fold
    attributes
    ~init:node
    ~style:(fun node _ _ x ->
      node#setStyle x;
      node)
    ~native_events:(fun node _ _ x ->
      node#setEvents x;
      node)
    ~tab_index:(fun node _ _ x ->
      node#setTabIndex x;
      node)
    ~custom_events:(fun node _ _ _ -> node)


let make ?default_style:custom_default_style attribute_list =
  let attrs = make_attributes ?style:custom_default_style () in

  let handle f e = Event.Expert.handle (f e) in
  let handle_key f e =
    Event.Expert.handle
      (f
         { Node_events.Keyboard.key = Keyboard.of_keycode e.Node_events.Keyboard.keycode
         ; ctrl = Revery.Key.Keymod.isControlDown e.keymod
         ; shift = Revery.Key.Keymod.isShiftDown e.keymod
         ; event_params = e
         }) in

  List.iter attribute_list ~f:(function
    | Style style -> attrs.style <- style
    | Tab_index i -> attrs.tab_index <- Some i
    | Custom_event_handler (Left_click e) ->
      attrs.custom_events <- { attrs.custom_events with on_left_click = Some e }
    | Custom_event_handler (Right_click e) ->
      attrs.custom_events <- { attrs.custom_events with on_right_click = Some e }
    | Custom_event_handler (Any_click e) ->
      attrs.custom_events <- { attrs.custom_events with on_any_click = Some e }
    | Native_event_handler (Ref f) ->
      attrs.native_events <- { attrs.native_events with ref = Some (handle f) }
    | Native_event_handler (Mouse_down f) ->
      attrs.native_events <- { attrs.native_events with onMouseDown = Some (handle f) }
    | Native_event_handler (Mouse_move f) ->
      attrs.native_events <- { attrs.native_events with onMouseMove = Some (handle f) }
    | Native_event_handler (Mouse_up f) ->
      attrs.native_events <- { attrs.native_events with onMouseUp = Some (handle f) }
    | Native_event_handler (Mouse_wheel f) ->
      attrs.native_events <- { attrs.native_events with onMouseWheel = Some (handle f) }
    | Native_event_handler (Key_down f) ->
      attrs.native_events <- { attrs.native_events with onKeyDown = Some (handle_key f) }
    | Native_event_handler (Key_up f) ->
      attrs.native_events <- { attrs.native_events with onKeyUp = Some (handle_key f) }
    | Native_event_handler (Text_input f) ->
      attrs.native_events <- { attrs.native_events with onTextInput = Some (handle f) }
    | Native_event_handler (Text_edit f) ->
      attrs.native_events <- { attrs.native_events with onTextEdit = Some (handle f) }
    | Native_event_handler (Mouse_enter f) ->
      attrs.native_events <- { attrs.native_events with onMouseEnter = Some (handle f) }
    | Native_event_handler (Mouse_leave f) ->
      attrs.native_events <- { attrs.native_events with onMouseLeave = Some (handle f) }
    | Native_event_handler (Mouse_over f) ->
      attrs.native_events <- { attrs.native_events with onMouseOver = Some (handle f) }
    | Native_event_handler (Mouse_out f) ->
      attrs.native_events <- { attrs.native_events with onMouseOut = Some (handle f) }
    | Native_event_handler (Dimensions_changed f) ->
      attrs.native_events <- { attrs.native_events with onDimensionsChanged = Some (handle f) }
    | Native_event_handler (Blur a) ->
      attrs.native_events <- { attrs.native_events with onBlur = Some (handle (fun () -> a)) }
    | Native_event_handler (Focus a) ->
      attrs.native_events <- { attrs.native_events with onFocus = Some (handle (fun () -> a)) });

  attrs


let node_ref f = Native_event_handler (Ref f)
let on_mouse_down f = Native_event_handler (Mouse_down f)
let on_mouse_move f = Native_event_handler (Mouse_move f)
let on_mouse_up f = Native_event_handler (Mouse_up f)
let on_mouse_wheel f = Native_event_handler (Mouse_wheel f)
let on_key_down f = Native_event_handler (Key_down f)
let on_key_up f = Native_event_handler (Key_up f)
let on_text_input f = Native_event_handler (Text_input f)
let on_text_edit f = Native_event_handler (Text_edit f)
let on_mouse_enter f = Native_event_handler (Mouse_enter f)
let on_mouse_leave f = Native_event_handler (Mouse_leave f)
let on_mouse_over f = Native_event_handler (Mouse_over f)
let on_mouse_out f = Native_event_handler (Mouse_out f)
let on_dimensions_changed f = Native_event_handler (Dimensions_changed f)
let on_focus e = Native_event_handler (Focus e)
let on_blur e = Native_event_handler (Blur e)
let on_click e = Custom_event_handler (Left_click e)
let on_right_click e = Custom_event_handler (Right_click e)
let on_any_click e = Custom_event_handler (Any_click e)
let tab_index i = Tab_index i
let style l = Style (UI.Style.create ~default:default_style ~style:l ())
