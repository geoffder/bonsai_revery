open Core_kernel
open Import
open Bonsai.Infix
module Attr = Attributes

let mouse_capture
    ?(on_mouse_down = fun state _evt -> Event.no_op, Some state)
    ?(on_mouse_up = fun state _evt -> Event.no_op, Some state)
    ?(on_mouse_move = fun state _evt -> Event.no_op, Some state)
    ?(on_mouse_wheel = fun state _evt -> Event.no_op, Some state)
    ?(on_release = fun _state -> Event.no_op)
    initial_state
  =
  let state = ref (Some initial_state) in
  let wrap f mouse_event =
    ( match !state with
    | Some s ->
      let event, new_state = f s mouse_event in
      Event.Expert.handle event;
      state := new_state
    | None -> () );
    if Option.is_none !state then Revery_UI.Mouse.releaseCapture () in
  Revery_UI.Mouse.setCapture
    (Option.value_exn (Revery_UI.getActiveWindow ()))
    ~onMouseDown:(wrap on_mouse_down)
    ~onMouseUp:(wrap on_mouse_up)
    ~onMouseMove:(wrap on_mouse_move)
    ~onMouseWheel:(wrap on_mouse_wheel)
    ~onRelease:(fun () -> on_release !state |> Event.Expert.handle)


let make_native_component constructor attributes f hooks =
  let open UI.React in
  let children, hooks = f hooks in
  ( { make = (fun () -> Attr.update_node attributes (constructor ()))
    ; configureInstance = (fun ~isFirstRender:_ -> Attr.update_node attributes)
    ; children
    ; insertNode
    ; deleteNode
    ; moveNode
    }
  , hooks )


let native_box =
  let component =
    UI.React.Expert.nativeComponent ~useDynamicKey:false (Source_code_position.to_string [%here])
  in

  fun attributes children ->
    component
      (make_native_component
         (Revery_UI_Primitives.PrimitiveNodeFactory.get ()).createViewNode
         attributes
         (Tuple2.create children))


let clickable_box' =
  let is_mouse_captured = ref false in
  let module Log = (val Log.with_namespace (Source_code_position.to_string [%here])) in
  fun ?key ?(disabled = false) component attributes f ->
    component
      ?key
      (let%hook children = f in
       let%hook is_mouse_captured_here = UI.React.Hooks.ref false in

       if not disabled
       then begin
         let capture () =
           if not !is_mouse_captured
           then (
             Log.trace "Capture";
             is_mouse_captured_here := true;
             is_mouse_captured := true ) in
         let release_capture () =
           if !is_mouse_captured_here
           then (
             Log.trace "Release";
             is_mouse_captured_here := false;
             is_mouse_captured := false ) in

         let on_mouse_up (mouse_evt : Node_events.Mouse_button.t) =
           if !is_mouse_captured_here
           then (
             release_capture ();

             let events =
               attributes.Attr.custom_events.on_any_click
               ::
               ( match mouse_evt.button with
               | Revery.MouseButton.BUTTON_LEFT -> [ attributes.custom_events.on_left_click ]
               | Revery.MouseButton.BUTTON_RIGHT -> [ attributes.custom_events.on_right_click ]
               | _ -> [] ) in

             Event.Many (List.filter_opt events) )
           else Event.no_op in

         let user_on_mouse_leave = attributes.native_events.onMouseLeave in
         let user_on_mouse_down = attributes.native_events.onMouseDown in
         let user_on_mouse_up = attributes.native_events.onMouseUp in

         attributes.native_events
           <- { attributes.native_events with
                onMouseLeave =
                  Some
                    (fun e ->
                      release_capture ();

                      Option.iter user_on_mouse_leave ~f:(fun f -> f e))
              ; onMouseDown =
                  Some
                    (fun e ->
                      capture ();
                      Option.iter user_on_mouse_down ~f:(fun f -> f e))
              ; onMouseUp =
                  Some
                    (fun e ->
                      Option.iter user_on_mouse_up ~f:(fun f -> f e);

                      on_mouse_up e |> Event.Expert.handle)
              };

         attributes.style
           <- { attributes.style with
                cursor =
                  Option.first_some attributes.style.cursor (Some Revery.MouseCursors.pointer)
              }
       end;

       native_box attributes children)


let has_click_events attributes =
  match attributes.Attr.custom_events with
  | { on_left_click = Some _; _ } | { on_right_click = Some _; _ } | { on_any_click = Some _; _ } ->
    true
  | _ -> false


module Expert = struct
  type 'a component =
    ?key:UI.React.Key.t
    -> (('a, 'a) UI.React.Hooks.t -> Element.t * (UI.React.Hooks.nil, 'a) UI.React.Hooks.t)
    -> Element.t

  let make_component ~use_dynamic_key =
    UI.React.Expert.component
      ~useDynamicKey:use_dynamic_key
      (Source_code_position.to_string [%here])


  let dyn_native_box_hooks ?key component attributes f =
    component
      ?key
      (let%hook children = f in
       native_box attributes children)


  let box ?(key : int option) (component : 'a component) attribute_list f =
    let f = Fn.compose (Tuple2.map_fst ~f:UI.React.listToElement) f in
    let key : UI.React.Key.t option = Obj.magic key in
    let attributes = Attr.make attribute_list in
    if has_click_events attributes
    then Obj.magic clickable_box' ?key component attributes f
    else dyn_native_box_hooks ?key component attributes f
end

let clickable_box =
  let component =
    UI.React.Expert.component ~useDynamicKey:false (Source_code_position.to_string [%here]) in
  fun ?key ?disabled attributes children ->
    clickable_box' ?key ?disabled component attributes (Tuple2.create children)


let box attribute_list children =
  let attributes = Attr.make attribute_list in
  if has_click_events attributes
  then clickable_box attributes (UI.React.listToElement children)
  else native_box attributes (UI.React.listToElement children)


let text' ~use_dynamic_key name =
  let component = UI.React.Expert.nativeComponent ~useDynamicKey:use_dynamic_key name in

  fun ?(key : int option) attributes text ->
    let key : UI.React.Key.t option = Obj.magic key in
    component ?key (fun hooks ->
        let textNode = (Revery_UI_Primitives.PrimitiveNodeFactory.get ()).createTextNode text in
        let open UI.React in
        ( { make = (fun () -> Obj.magic (Attr.update_text_node attributes textNode))
          ; configureInstance =
              (fun ~isFirstRender:_ node ->
                let text_node : Revery_UI.textNode = Obj.magic node in
                text_node#setText text;
                Obj.magic (Attr.update_text_node attributes text_node))
          ; children = UI.React.empty
          ; insertNode
          ; deleteNode
          ; moveNode
          }
        , hooks ))


let text =
  let text' = text' ~use_dynamic_key:false (Source_code_position.to_string [%here]) ?key:None in
  fun attribute_list text ->
    let attributes = Attr.make attribute_list in
    text' attributes text


let image' ~use_dynamic_key name =
  let component = UI.React.Expert.nativeComponent ~useDynamicKey:use_dynamic_key name in

  fun ?(key : int option) attributes ->
    let key : UI.React.Key.t option = Obj.magic key in
    component ?key (fun hooks ->
        let imageNode = (Revery_UI_Primitives.PrimitiveNodeFactory.get ()).createImageNode None in
        let open UI.React in
        ( { make = (fun () -> Obj.magic (Attr.update_image_node attributes imageNode))
          ; configureInstance =
              (fun ~isFirstRender:_ node ->
                Obj.magic (Attr.update_image_node attributes (Obj.magic node)))
          ; children = UI.React.empty
          ; insertNode
          ; deleteNode
          ; moveNode
          }
        , hooks ))


let image =
  let image' = image' ~use_dynamic_key:false (Source_code_position.to_string [%here]) ?key:None in
  fun attribute_list ->
    let attributes = Attr.make attribute_list in
    image' attributes


let opacity =
  let component =
    UI.React.Expert.nativeComponent ~useDynamicKey:false (Source_code_position.to_string [%here])
  in

  fun ?(opacity = 1.0) children ->
    component
      (make_native_component
         (Revery_UI_Primitives.PrimitiveNodeFactory.get ()).createNode
         Attr.(make [ style [ `Opacity opacity ] ])
         (Tuple2.create (UI.React.listToElement children)))


type ticker =
  { mutable intervals : Time.Span.t list
  ; mutable clear_active_interval : unit -> unit
  }

let tick =
  let empty_interval () = () in
  let ticker = { intervals = []; clear_active_interval = empty_interval } in
  let restart_interval () =
    ticker.clear_active_interval ();
    match ticker.intervals with
    | [] -> ticker.clear_active_interval <- empty_interval
    | interval :: _ ->
      ticker.clear_active_interval
        <- Revery.Tick.interval
             ~name:"tick stabilize"
             (fun _ ->
               Incr.Clock.advance_clock Incr.clock ~to_:(Time_ns.now ());
               Lwt.wakeup_paused ();
               Lwt_engine.iter false;
               Timber.Log.perf "tick stabilize" Incr.stabilize)
             (Revery.Time.ofFloatSeconds (Time.Span.to_sec interval)) in

  let add_interval interval =
    let restart =
      match ticker.intervals with
      | [] -> true
      | x :: _ -> Time.Span.(interval < x) in
    ticker.intervals <- interval :: ticker.intervals |> List.sort ~compare:Time.Span.compare;
    if restart then restart_interval () in

  let remove_interval interval =
    let restart =
      match ticker.intervals with
      | a :: b :: _ when Time.Span.(a = interval && a <> b) -> true
      | [ _ ] | [] -> true
      | _ -> false in

    let rec loop acc found xs =
      match xs with
      | [] -> List.rev acc
      | x :: xs when found || Time.Span.(x <> interval) -> loop (x :: acc) found xs
      | _ :: xs -> loop acc true xs in

    ticker.intervals <- loop [] false ticker.intervals;

    if restart then restart_interval () in

  let component =
    UI.React.Expert.component ~useDynamicKey:false (Source_code_position.to_string [%here]) in

  fun node ~every ->
    component
      (let%hook () =
         UI.React.Hooks.effect
           (OnMountAndIf ((fun a b -> Time.Span.(a <> b)), every))
           (fun () ->
             add_interval every;

             Some (fun () -> remove_interval every)) in

       node)


let compose_event_handler ~f = function
  | None -> Some f
  | Some e ->
    Some
      (fun x ->
        f x;
        e x)


let button =
  let component =
    UI.React.Expert.component ~useDynamicKey:false (Source_code_position.to_string [%here]) in
  let text = text' ~use_dynamic_key:false (Source_code_position.to_string [%here]) ?key:None in
  fun ?(disabled = false) ?disabled_attr attribute_fn title ->
    component
      (let%hook hovered, set_hovered = UI.React.Hooks.state false in
       let attribute_list = attribute_fn ~hovered:((not disabled) && hovered) in
       let attribute_list =
         match disabled_attr with
         | Some disabled_attr -> List.append attribute_list disabled_attr
         | _ -> attribute_list in

       let attributes = Attr.make attribute_list in
       let on_mouse_over =
         compose_event_handler attributes.native_events.onMouseOver ~f:(fun _ ->
             set_hovered (fun _ -> true)) in
       let on_mouse_out =
         compose_event_handler attributes.native_events.onMouseOut ~f:(fun _ ->
             set_hovered (fun _ -> false)) in
       attributes.native_events
         <- { attributes.native_events with onMouseOver = on_mouse_over; onMouseOut = on_mouse_out };

       let text_attributes =
         let style = attributes.style in
         let style =
           UI.Style.make
             ~lineHeight:style.lineHeight
             ~textWrap:style.textWrap
             ~textOverflow:style.textOverflow
             ~color:style.color
             () in
         Attr.make_attributes ~style ~kind:attributes.kind () in

       clickable_box ~disabled attributes (text text_attributes title))


module Text_input = struct
  type props =
    { autofocus : bool
    ; cursor_color : (Color.t[@sexp.opaque])
    ; placeholder : string
    ; placeholder_color : (Color.t[@sexp.opaque])
    ; default_value : string option
    ; on_key_down : Node_events.Keyboard.t -> string -> (string -> Event.t) -> Event.t
    ; attributes : Attr.t list
    }
  [@@deriving sexp_of]

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
      >>| fun time ->
      Int.rem (Time_ns.to_span_since_epoch time |> Time_ns.Span.to_int_ms) 1000 > 500 in

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
end

module Resizable = struct
  type resize =
    [ `Scale of float option * float option
    | `Set of int option * int option
    ]
  [@@deriving sexp_of]

  type props =
    { styles : (Style.t[@sexp.opaque]) list
    ; attributes : Attr.t list
    ; max_width : int option
    ; max_height : int option
    }
  [@@deriving sexp_of]

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
end

module Draggable = struct
  type freedom =
    | X
    | Y
    | Free
  [@@deriving sexp_of]

  type props =
    { styles : (Style.t[@sexp.opaque]) list
    ; attributes : Attr.t list
    ; freedom : freedom
    ; snap_back : bool
    ; on_drag : bb:BoundingBox2d.t -> x:float -> y:float -> Event.t
    ; on_drop : bb:BoundingBox2d.t -> x:float -> y:float -> Event.t
    }
  [@@deriving sexp_of]

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
              Float.clamp_exn
                (x1 -. x0)
                ~min:(o_l -. corner_x)
                ~max:(o_r -. corner_x +. (i_l -. i_r))
            | Y -> 0. in
          let y =
            match props.freedom with
            | Free | Y ->
              Float.clamp_exn
                (y1 -. y0)
                ~min:(o_t -. corner_y)
                ~max:(o_b -. corner_y +. (i_t -. i_b))
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
            shift props.freedom inner_box model.outer_box x_pos y_pos (x_pos +. x) (y_pos +. y)
          in
          { model with x_trans = shift_x +. model.x_trans; y_trans = shift_y +. model.y_trans } )
      | InnerBox bb -> { model with inner_box = Some bb }
      | OuterBox bb -> { model with outer_box = Some bb }
      | Set_node node -> { model with node = Some node }
  end

  let component = Bonsai.of_module (module T) ~default_model:T.Model.default
end

module Slider = struct
  type length =
    | Dynamic of int
    | Static of int
  [@@deriving sexp_of]

  let length_val = function
    | Dynamic v -> v
    | Static v -> v


  let length_to_styles vertical reverse thickness = function
    | Static i ->
      Style.
        [ width (if vertical then thickness else i); height (if vertical then i else thickness) ]
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
    ; track_color : (Color.t[@sexp.opaque])
    ; thumb : (Draggable.props[@sexp.opaque])
    }
  [@@deriving sexp_of]

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
end

module ScrollView = struct
  type props =
    { speed : float
    ; styles : (Style.t[@sexp.opaque]) list
    ; attributes : Attr.t list
    ; min_thumb_size : int
    ; x_slider : Slider.props
    ; y_slider : Slider.props
    }
  [@@deriving sexp_of]

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
          let elements =
            view :: (if Float.(scroll_height > 0.) then [ sliders.y.element ] else []) in
          box Attr.[ style styles ] elements in
        if Float.(scroll_width > 0.) then box [] [ inner_box; sliders.x.element ] else inner_box
      in
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
end

module Text_area = struct
  type props =
    { autofocus : bool
    ; text_color : (Color.t[@sexp.opaque])
    ; cursor_color : (Color.t[@sexp.opaque])
    ; placeholder : string
    ; placeholder_color : (Color.t[@sexp.opaque])
    ; default_value : string option
    ; on_key_down : Node_events.Keyboard.t -> string -> (string -> Event.t) -> Event.t
    ; max_height : int
    ; attributes : Attr.t list
    }
  [@@deriving sexp_of]

  let props
      ?(autofocus = false)
      ?(text_color = Colors.black)
      ?(cursor_color = Revery.UI.Components.Input.Styles.defaultCursorColor)
      ?(placeholder = "")
      ?(placeholder_color = Revery.UI.Components.Input.Styles.defaultPlaceholderColor)
      ?default_value
      ?(on_key_down = fun _ _ _ -> Event.no_op)
      ?(max_height = Int.max_value)
      attributes
    =
    { autofocus
    ; text_color
    ; cursor_color
    ; placeholder
    ; placeholder_color
    ; default_value
    ; on_key_down
    ; max_height
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
        ; text_node : (UI.node[@sexp.opaque] [@equal.ignore]) option
        ; input_node : (UI.node[@sexp.opaque] [@equal.ignore]) option
        ; cursor_position : int
        ; x_offset : float
        ; y_offset : float
        ; x_scroll : float
        ; y_scroll : float
        }
      [@@deriving equal, sexp]

      let default =
        { focused = false
        ; value = None
        ; text_node = None
        ; input_node = None
        ; cursor_position = 0
        ; x_offset = 0.
        ; y_offset = 0.
        ; x_scroll = 0.
        ; y_scroll = 0.
        }
    end

    module Action = struct
      type t =
        | Focus
        | Blur
        | Text_input of string * int
        | Set_value of string
        | UpdateOffsets
        | Set_text_node of (UI.node[@sexp.opaque])
        | Set_input_node of (UI.node[@sexp.opaque])
      [@@deriving sexp_of]
    end

    module Result = struct
      type t = string * (string -> Event.t) * Element.t
    end

    let name = "InputArea"

    let styles =
      Style.
        [ cursor `Text
        ; flex_direction `Row (* ; align_items `FlexStart *)
        ; align_items `Center
        ; margin_left 10
        ; margin_right 10
        ; flex_grow 1
        ; justify_content `FlexStart
        ]


    let default_text_spec = { Attr.KindSpec.Text.default with size = 18. }
    let default_kind = Attr.KindSpec.(TextNode default_text_spec)

    let text_dimensions (font_info : Attr.KindSpec.Text.t) text =
      Revery_Draw.Text.dimensions
        ~smoothing:Revery.Font.Smoothing.default
        ~fontFamily:font_info.family
        ~fontSize:font_info.size
        ~fontWeight:font_info.weight
        text


    let measure_text_width font_info text =
      let dims = text_dimensions font_info text in
      dims.width


    let measure_text_height (font_info : Attr.KindSpec.Text.t) =
      Revery_Draw.Text.lineHeight
        ~italic:font_info.italicized
        font_info.family
        font_info.size
        font_info.weight


    let get_line_height font_info node =
      let style : UI.Style.t = node#getStyle () in
      style.lineHeight *. measure_text_height font_info


    let get_font_info (attrs : Attr.t list) =
      List.find_map
        ~f:(function
            | Kind (TextNode info) -> Some info
            | _ -> None)
        attrs
      |> Option.value ~default:default_text_spec


    (* FIXME: Need to deal with really long word case, where the container
     * is overflown by a long unbroken "word". It should be broken up somehow, otherwise
     * it's possible to extend out of the window. Don't really want to introduce newlines
     * to the actual value though, should only do at the display stage. Honestly, should
     * think of doing a PR on the Revery wrapping side to make wrapping like I want an
     * option. *)
    let measure_text_dims font_info line_height margin text =
      let measure_width = measure_text_width font_info in
      let lines = String.split_lines text in
      let max_x_offset, x_offset, y_count =
        List.fold
          ~init:(0., 0., Int.max 0 (List.length lines - 1))
          ~f:(fun (max_x, x, total_y) line ->
            let inner (acc, y) word =
              let ext = acc ^ " " ^ word in
              if Float.(measure_width ext > margin) then word, y + 1 else ext, y in
            let line_x, line_y =
              match String.split ~on:' ' line with
              | [] -> 0., 0
              | [ h ] -> measure_width h, 0
              | h :: t ->
                let l, y' = List.fold ~init:(h, 0) ~f:inner t in
                measure_width l, y' in
            Float.max max_x line_x, line_x, line_y + total_y)
          lines in
      let y_offset = Float.(of_int y_count * line_height) in
      if (not (String.is_empty text)) && Char.equal '\n' text.[String.length text - 1]
      then max_x_offset, 0., y_offset +. line_height
      else max_x_offset, x_offset, y_offset


    (* FIXME: There is some glitchyness as scrolling occurs when the container
     * is growing. Not sure where the origin is, obv probably not here, but need
     * to investigate. *)
    let vertical_scroll container_height text_height line_height y_offset y_scroll =
      let open Float in
      if text_height > container_height
      then (
        let offset =
          if y_offset < y_scroll
          then y_offset
          else if y_offset + line_height - y_scroll > container_height
          then y_offset + line_height - container_height
          else y_scroll in
        (* Make sure all the space is used to show text (no overscroll). *)
        Float.clamp_exn ~min:0. ~max:(text_height -. container_height) offset )
      else 0.


    let horizontal_scroll margin text_width x_offset x_scroll =
      let open Float in
      if text_width > margin
      then (
        let offset =
          if x_offset < x_scroll
          then x_offset
          else if x_offset - x_scroll > margin
          then x_offset - margin
          else x_scroll in
        (* Make sure all the space is used to show text (no overscroll). *)
        Float.clamp_exn ~min:0. ~max:(text_width -. margin) offset )
      else 0.


    let index_nearest_offset ~measure x_offset y_offset text =
      let length = String.length text in
      (* necessary? harmful? *)
      let x_offset = Float.max 0. x_offset in
      let rec find_row_start ~last_y ~last_start i =
        if i > length
        then last_y, last_start
        else (
          let _, _, height = measure (String.sub text 0 i) in
          if Float.(height > y_offset)
          then (
            let start = if Float.(height - y_offset < y_offset - last_y) then i else last_start in
            last_y, start )
          else (
            let last_y, last_start =
              if Float.(height > last_y) then height, i else last_y, last_start in
            find_row_start ~last_y ~last_start (i + 1) ) ) in
      let row_y, row_i = find_row_start ~last_y:0. ~last_start:0 1 in
      let rec loop ~last_x i =
        if i > length
        then length
        else (
          let _, width, height = measure (String.sub text 0 i) in
          if Float.(height > row_y)
          then i - 1 (* last char of searched row *)
          else if Float.(width > x_offset)
          then if Float.(width - x_offset < x_offset - last_x) then i else i - 1
          else loop ~last_x:width (i + 1) ) in
      loop ~last_x:0. row_i


    let vertical_nav ~up text_node font_info start_position text =
      match text_node, Option.bind text_node ~f:(fun n -> n#getParent ()) with
      | Some node, Some parent ->
        let container : UI.Dimensions.t = parent#measurements () in
        let line_height = get_line_height font_info node in
        let measure = measure_text_dims font_info line_height (Float.of_int container.width) in
        let _, target_x, start_y = measure (String.sub text 0 start_position) in
        let target_y = start_y +. if up then -.line_height else line_height in
        let cursor_position = index_nearest_offset ~measure target_x target_y text in
        cursor_position
      | _ -> start_position


    (* Add spaces between consecutive newline characters to ensure they are not collapsed/combined
       by Revery text wrapping. *)
    let newline_hack text =
      let f (was_newline, acc) c =
        let is_newline = Char.equal '\n' c in
        is_newline, if was_newline && is_newline then acc ^ " \n" else acc ^ String.of_char c in
      let _, hacked = String.fold ~f ~init:(false, "") text in
      hacked


    let end_chars = Set.of_list (module Char) [ '\n'; ' '; '/'; '_'; '-'; ','; '.'; ';'; '"' ]

    let chars_to_next_word_end str =
      let len = String.length str in
      if len > 0
      then (
        let rec loop i = if i = len || Set.mem end_chars str.[i] then i else loop (i + 1) in
        loop 1 )
      else 0


    let chars_to_previous_word_end str =
      let len = String.length str in
      if len > 0
      then (
        let sub_len = len - 1 in
        let rec loop i =
          if i = len || Set.mem end_chars str.[sub_len - i] then i else loop (i + 1) in
        loop 1 )
      else 0


    let remove_word_before text cursor_position =
      let open Revery.UI.Components.Input in
      let before, after = getStringParts cursor_position text in
      if String.length before > 0
      then (
        let next_position = cursor_position - chars_to_previous_word_end before in
        let new_text = Str.string_before before next_position ^ after in
        new_text, next_position )
      else after, cursor_position


    let remove_word_after text cursor_position =
      let open Revery.UI.Components.Input in
      let before, after = getStringParts cursor_position text in
      let new_text =
        if String.length after > 0
        then Str.string_after after (chars_to_next_word_end after)
        else before in
      new_text, cursor_position


    let compute ~inject ((cursor_on, props) : Input.t) (model : Model.t) =
      let open Revery.UI.Components.Input in
      let font_info = get_font_info props.attributes in
      let value = Option.first_some model.value props.default_value |> Option.value ~default:"" in
      let show_placeholder = String.equal value "" in
      let cursor_position = model.cursor_position in

      let measure_text_width text = measure_text_width font_info text in
      let set_value value = inject (Action.Set_value value) in
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
            let cursor_position =
              if keyboard_event.ctrl
              then (
                let before, _ = getStringParts cursor_position value in
                cursor_position - chars_to_previous_word_end before )
              else getSafeStringBounds value cursor_position (-1) in
            inject (Action.Text_input (value, cursor_position))
          | Right ->
            let cursor_position =
              if keyboard_event.ctrl
              then (
                let _, after = getStringParts cursor_position value in
                cursor_position + chars_to_next_word_end after )
              else getSafeStringBounds value cursor_position 1 in
            inject (Action.Text_input (value, cursor_position))
          | Up ->
            let cursor_position =
              vertical_nav ~up:true model.text_node font_info cursor_position value in
            update value cursor_position
          | Down ->
            let cursor_position =
              vertical_nav ~up:false model.text_node font_info cursor_position value in
            update value cursor_position
          | Delete ->
            let value, cursor_position =
              if keyboard_event.ctrl
              then remove_word_after value cursor_position
              else removeCharacterAfter value cursor_position in
            inject (Action.Text_input (value, cursor_position))
          | Backspace ->
            let value, cursor_position =
              if keyboard_event.ctrl
              then remove_word_before value cursor_position
              else removeCharacterBefore value cursor_position in
            inject (Action.Text_input (value, cursor_position))
          | V when keyboard_event.ctrl -> paste value cursor_position
          | Return when keyboard_event.shift ->
            let value, cursor_position = insertString value "\n" cursor_position in
            update value cursor_position
          | Escape ->
            UI.Focus.loseFocus ();
            Event.no_op
          | _ -> Event.no_op in
        Event.Many [ event; props.on_key_down keyboard_event value set_value ] in

      let handle_click (event : Node_events.Mouse_button.t) =
        match model.text_node, Option.bind model.text_node ~f:(fun n -> n#getParent ()) with
        | Some node, Some parent ->
          let container : UI.Dimensions.t = parent#measurements () in
          let line_height = get_line_height font_info node in
          let measure = measure_text_dims font_info line_height (Float.of_int container.width) in
          let scene_offsets = (node#getSceneOffsets () : UI.Offset.t) in
          let x_text_offset = event.mouseX -. Float.of_int scene_offsets.left in
          let y_text_offset = event.mouseY -. Float.of_int scene_offsets.top +. model.y_scroll in
          let cursor_position = index_nearest_offset ~measure x_text_offset y_text_offset value in
          Option.iter model.input_node ~f:UI.Focus.focus;
          update value cursor_position
        | _ -> Event.no_op in

      let cursor =
        let x_offset = model.x_offset -. model.x_scroll in
        let y_offset = model.y_offset -. model.y_scroll in
        let position_style =
          Style.
            [ position `Absolute
            ; margin_top 2
            ; transform [ TranslateX x_offset; TranslateY y_offset ]
            ] in
        tick ~every:(if model.focused then Time.Span.of_ms 16.0 else Time.Span.of_hr 1.0)
        @@ box
             Attr.[ style position_style ]
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
          :: style Style.(max_height props.max_height :: styles)
          :: props.attributes) in

      let view =
        box
          attributes
          [ box
              Attr.
                [ style Style.[ flex_grow 1; margin_right (Int.of_float @@ measure_text_width "_") ]
                ; on_dimensions_changed (fun d -> inject Action.UpdateOffsets)
                ]
              [ text
                  Attr.
                    [ node_ref (fun node -> inject (Action.Set_text_node node))
                    ; style
                        Style.
                          [ color
                              ( if show_placeholder
                              then props.placeholder_color
                              else props.text_color )
                          ; justify_content `FlexStart
                          ; align_items `Center
                          ; text_wrap WrapIgnoreWhitespace
                          ; transform
                              [ TranslateX (-.model.x_scroll); TranslateY (-.model.y_scroll) ]
                          ; min_height (Int.of_float (measure_text_height font_info))
                          ]
                    ; kind (TextNode font_info)
                    ]
                  (if show_placeholder then props.placeholder else newline_hack value)
              ]
          ; cursor
          ] in

      value, set_value, view


    let apply_action ~inject ~schedule_event ((_, props) : Input.t) (model : Model.t) = function
      | Action.Focus ->
        Sdl2.TextInput.start ();
        { model with focused = true }
      | Blur ->
        Sdl2.TextInput.stop ();
        { model with focused = false }
      | Text_input (value, cursor_position) ->
        schedule_event (inject Action.UpdateOffsets);
        { model with value = Some value; cursor_position }
      | Set_value value ->
        let cursor_position = min model.cursor_position (String.length value) in
        schedule_event (inject Action.UpdateOffsets);
        { model with value = Some value; cursor_position }
      | UpdateOffsets ->
        let value = Option.value ~default:"" model.value in
        let cursor_position = min model.cursor_position (String.length value) in
        ( match Option.bind model.text_node ~f:(fun n -> n#getParent ()) with
        | Some node ->
          let font_info = get_font_info props.attributes in
          let container : UI.Dimensions.t = node#measurements () in
          let line_height = get_line_height font_info node in
          let _, x_offset, y_offset =
            measure_text_dims
              font_info
              line_height
              (Float.of_int container.width)
              (String.sub ~pos:0 ~len:cursor_position value) in
          let max_x_offset, _, max_y_offset =
            measure_text_dims font_info line_height (Float.of_int container.width) value in
          let x_scroll =
            horizontal_scroll
              (Float.of_int container.width -. measure_text_width font_info "_")
              max_x_offset
              x_offset
              model.x_scroll in
          let y_scroll =
            vertical_scroll
              (Float.of_int container.height)
              (max_y_offset +. line_height)
              line_height
              y_offset
              model.y_scroll in
          { model with x_offset; y_offset; x_scroll; y_scroll }
        | None -> model )
      | Set_text_node node -> { model with text_node = Some node }
      | Set_input_node node -> { model with input_node = Some node }
  end

  let component =
    let cursor_on =
      Bonsai.With_incr.of_incr (Incr.Clock.watch_now Incr.clock)
      >>| fun time ->
      Int.rem (Time_ns.to_span_since_epoch time |> Time_ns.Span.to_int_ms) 1000 > 500 in

    let component = Bonsai.of_module (module T) ~default_model:T.Model.default in

    let cutoff =
      Bonsai.With_incr.value_cutoff
        ~cutoff:
          (Incr.Cutoff.create
             (fun ~old_value:(old_timer, old_input) ~new_value:(new_timer, new_input) ->
               Bool.equal old_timer new_timer && phys_equal old_input new_input)) in

    ignore @>> cursor_on |> Bonsai.Arrow.extend_first >>> cutoff >>> component
end
