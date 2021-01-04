open Core_kernel
open Import
open Bonsai.Infix
open Components
module Attr = Attributes

type props =
  { autofocus : bool
  ; cursor_color : Color.t
  ; placeholder : string
  ; placeholder_color : Color.t
  ; default_value : string option
  ; on_key_down : Node_events.Keyboard.t -> string -> (string -> Event.t) -> Event.t
  ; attributes : Attr.t list
  }

let props
    ?(autofocus = false)
    ?(cursor_color = Revery.UI.Components.Input.Styles.defaultCursorColor)
    ?(placeholder = "")
    ?(placeholder_color = Revery.UI.Components.Input.Styles.defaultPlaceholderColor)
    ?default_value
    ?(on_key_down = fun _ _ _ -> Event.no_op)
    attributes
  =
  { autofocus
  ; cursor_color
  ; placeholder
  ; placeholder_color
  ; default_value
  ; on_key_down
  ; attributes
  }


module T = struct
  module Input = struct
    type t = bool * props
  end

  module Model = struct
    type t =
      { focused : bool
      ; value : string option
      ; cursor_position : int
      ; text_node : (UI.node[@sexp.opaque] [@equal.ignore]) option
      ; input_node : (UI.node[@sexp.opaque] [@equal.ignore]) option
      ; scroll_offset : int ref
      }
    [@@deriving equal, sexp]
  end

  module Action = struct
    type t =
      | Focus
      | Blur
      | Text_input of string * int
      | Set_value of string
      | Set_text_node of (UI.node[@sexp.opaque])
      | Set_input_node of (UI.node[@sexp.opaque])
    [@@deriving sexp_of]
  end

  module Result = struct
    type t = string * (string -> Event.t) * Element.t
  end

  let name = "Input"

  let default_style =
    let open Revery.UI.LayoutTypes in
    { Attr.default_style with
      color = Colors.black
    ; cursor = Some Revery.MouseCursors.text
    ; flexDirection = Revery.UI.LayoutTypes.Row
    ; alignItems = AlignCenter
    ; justifyContent = JustifyFlexStart
    ; overflow = Hidden
    }


  let default_kind = Attr.KindSpec.(TextNode { Text.default with size = 18. })

  let compute ~inject ((cursor_on, props) : Input.t) (model : Model.t) =
    let open Revery.UI.Components.Input in
    let attributes = Attr.make ~default_style ~default_kind props.attributes in
    let font_info =
      match attributes.kind with
      | TextNode spec -> spec
      | _ -> Attr.KindSpec.Text.default in
    let value = Option.first_some model.value props.default_value |> Option.value ~default:"" in
    let set_value value = inject (Action.Set_value value) in
    let show_placeholder = String.equal value "" in
    let scroll_offset = model.scroll_offset in
    let cursor_position = min model.cursor_position (String.length value) in

    let measure_text_width text =
      let dimensions =
        Revery_Draw.Text.dimensions
          ~smoothing:Revery.Font.Smoothing.default
          ~fontFamily:font_info.family
          ~fontSize:font_info.size
          ~fontWeight:font_info.weight
          text in
      Float.to_int dimensions.width in

    let () =
      let cursor_offset = measure_text_width (String.sub value ~pos:0 ~len:cursor_position) in

      match Option.bind model.text_node ~f:(fun node -> node#getParent ()) with
      | Some containerNode ->
        let container = (containerNode#measurements () : UI.Dimensions.t) in
        if cursor_offset < !scroll_offset
        then scroll_offset := cursor_offset
        else if cursor_offset - !scroll_offset > container.width
        then scroll_offset := cursor_offset - container.width
      | None -> () in

    let update value cursor_position = inject (Action.Text_input (value, cursor_position)) in

    let paste value cursor_position =
      match Sdl2.Clipboard.getText () with
      | None -> Event.no_op
      | Some data ->
        let value, cursor_position = insertString value data cursor_position in
        update value cursor_position in

    let handle_text_input (event : Node_events.Text_input.t) =
      let value, cursor_position = insertString value event.text cursor_position in
      update value cursor_position in

    let handle_key_down (keyboard_event : Node_events.Keyboard.t) =
      let event =
        match keyboard_event.key with
        | Left ->
          let cursor_position = getSafeStringBounds value cursor_position (-1) in
          inject (Action.Text_input (value, cursor_position))
        | Right ->
          let cursor_position = getSafeStringBounds value cursor_position 1 in
          inject (Action.Text_input (value, cursor_position))
        | Delete ->
          let value, cursor_position = removeCharacterAfter value cursor_position in
          inject (Action.Text_input (value, cursor_position))
        | Backspace ->
          let value, cursor_position = removeCharacterBefore value cursor_position in
          inject (Action.Text_input (value, cursor_position))
        | V when keyboard_event.ctrl -> paste value cursor_position
        | Escape ->
          UI.Focus.loseFocus ();
          Event.no_op
        | _ -> Event.no_op in
      Event.Many [ event; props.on_key_down keyboard_event value set_value ] in

    let handle_click (event : Node_events.Mouse_button.t) =
      match model.text_node with
      | Some node ->
        let sceneOffsets = (node#getSceneOffsets () : UI.Offset.t) in
        let textOffset = int_of_float event.mouseX - sceneOffsets.left + !scroll_offset in
        let cursor_position =
          Revery_Draw.Text.indexNearestOffset ~measure:measure_text_width value textOffset in

        Option.iter model.input_node ~f:UI.Focus.focus;

        update value cursor_position
      | None -> Event.no_op in

    let cursor =
      let startStr, _ = getStringParts cursor_position value in
      let textWidth = measure_text_width startStr in
      let offset = textWidth - !scroll_offset in
      tick ~every:(if model.focused then Time.Span.of_ms 16.0 else Time.Span.of_hr 1.0)
      @@ box
           Attr.[ style (Styles.cursor ~offset) ]
           [ opacity
               ~opacity:(if model.focused && cursor_on then 1.0 else 0.0)
               [ box
                   Attr.
                     [ style
                         Style.
                           [ width Constants.cursorWidth
                           ; height (Float.to_int font_info.size)
                           ; background_color props.cursor_color
                           ]
                     ]
                   []
               ]
           ] in

    let attributes =
      Attr.(
        node_ref (fun node ->
            if props.autofocus then UI.Focus.focus node;
            inject (Action.Set_input_node node))
        :: on_mouse_down handle_click
        :: on_key_down handle_key_down
        :: on_text_input handle_text_input
        :: on_focus (inject Action.Focus)
        :: on_blur (inject Action.Blur)
        :: props.attributes)
      |> Attr.make ~default_style ~default_kind in

    attributes.style
      <- { attributes.style with
           cursor = Option.first_some attributes.style.cursor (Some Revery.MouseCursors.text)
         };

    let view =
      clickable_box
        attributes
        (box
           Attr.[ style Styles.marginContainer ]
           [ box
               Attr.[ style Styles.textContainer ]
               [ text
                   Attr.
                     [ node_ref (fun node -> inject (Action.Set_text_node node))
                     ; style
                         (Styles.text
                            ~showPlaceholder:show_placeholder
                            ~scrollOffset:scroll_offset
                            ~placeholderColor:props.placeholder_color
                            ~color:attributes.style.color)
                     ; kind attributes.kind
                     ]
                   (if show_placeholder then props.placeholder else value)
               ]
           ; cursor
           ]) in

    value, set_value, view


  let apply_action ~inject:_ ~schedule_event:_ _ (model : Model.t) = function
    | Action.Focus ->
      (* resetCursor ();
       * onFocus (); *)
      Sdl2.TextInput.start ();

      { model with focused = true }
    | Blur ->
      (* resetCursor();
       * onBlur(); *)
      Sdl2.TextInput.stop ();

      { model with focused = false }
    | Text_input (value, cursor_position) -> { model with value = Some value; cursor_position }
    | Set_value value ->
      { model with
        value = Some value
      ; cursor_position = min model.cursor_position (String.length value)
      }
    | Set_text_node node -> { model with text_node = Some node }
    | Set_input_node node -> { model with input_node = Some node }
end

let component =
  let cursor_on =
    Bonsai.With_incr.of_incr (Incr.Clock.watch_now Incr.clock)
    >>| fun time -> Int.rem (Time_ns.to_span_since_epoch time |> Time_ns.Span.to_int_ms) 1000 > 500
  in

  let component =
    Bonsai.of_module
      (module T)
      ~default_model:
        T.Model.
          { focused = false
          ; value = None
          ; cursor_position = 0
          ; text_node = None
          ; input_node = None
          ; scroll_offset = ref 0
          } in

  let cutoff =
    Bonsai.With_incr.value_cutoff
      ~cutoff:
        (Incr.Cutoff.create
           (fun ~old_value:(old_timer, old_input) ~new_value:(new_timer, new_input) ->
             Bool.equal old_timer new_timer && phys_equal old_input new_input)) in

  ignore @>> cursor_on |> Bonsai.Arrow.extend_first >>> cutoff >>> component
