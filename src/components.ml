open Core_kernel
open Import
open Bonsai.Infix
module Attr = Attributes

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

             Event.Many (List.filter_map events ~f:Fn.id) )
           else Event.no_op in

         let user_on_mouse_leave = attributes.native_events.onMouseLeave in
         let user_on_mouse_down = attributes.native_events.onMouseDown in
         let user_on_mouse_up = attributes.native_events.onMouseUp in
         let user_on_mouse_wheel = attributes.native_events.onMouseWheel in

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
              ; onMouseWheel =
                  Some
                    (fun e ->
                      capture ();

                      Option.iter user_on_mouse_wheel ~f:(fun f -> f e))
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

    let compute ~inject ((cursor_on, input) : Input.t) (model : Model.t) =
      let open Revery.UI.Components.Input in
      let attributes = Attr.make ~default_style ~default_kind input.attributes in
      let font_info =
        match attributes.kind with
        | TextNode spec -> spec
        | _ -> Attr.KindSpec.Text.default in
      let value = Option.first_some model.value input.default_value |> Option.value ~default:"" in
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
          | Escape ->
            UI.Focus.loseFocus ();
            Event.no_op
          | _ -> Event.no_op in
        Event.Many [ event; input.on_key_down keyboard_event value set_value ] in

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
                             ; background_color input.cursor_color
                             ]
                       ]
                     []
                 ]
             ] in

      let attributes =
        Attr.(
          node_ref (fun node ->
              if input.autofocus then UI.Focus.focus node;
              inject (Action.Set_input_node node))
          :: on_mouse_down handle_click
          :: on_key_down handle_key_down
          :: on_text_input handle_text_input
          :: on_focus (inject Action.Focus)
          :: on_blur (inject Action.Blur)
          :: input.attributes)
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
                              ~placeholderColor:input.placeholder_color
                              ~color:attributes.style.color)
                       ; kind attributes.kind
                       ]
                     (if show_placeholder then input.placeholder else value)
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

module Draggable = struct
  type freedom =
    | Free
    | X
    | Y
  [@@deriving sexp_of]

  type props =
    { styles : (Style.t[@sexp.opaque]) list
    ; attributes : Attr.t list
    ; snap_back : bool
    ; freedom : freedom
    ; on_drag : x:float -> y:float -> Event.t
    ; on_drop : BoundingBox2d.t -> Event.t
    }
  [@@deriving sexp_of]

  let props
      ?(attributes = [])
      ?(snap_back = false)
      ?(freedom = Y)
      ?(on_drag = fun ~x:_ ~y:_ -> Event.no_op)
      ?(on_drop = fun _ -> Event.no_op)
      styles
    =
    { styles; attributes; snap_back; freedom; on_drag; on_drop }


  module T = struct
    module Model = struct
      type t =
        { start : (float * float) option
        ; x_trans : float
        ; y_trans : float
        ; bounding_box : (BoundingBox2d.t[@sexp.opaque])
        }
      [@@deriving equal, sexp]

      let default =
        { start = None
        ; x_trans = 0.
        ; y_trans = 0.
        ; bounding_box = BoundingBox2d.create 0. 0. 1. 1.
        }
    end

    module Action = struct
      type t =
        | Grab of float * float
        | Drop
        | Drag of float * float
        | Reset
        | SetBoundingBox of (BoundingBox2d.t[@sexp.opaque])
      [@@deriving sexp_of]
    end

    module Input = struct
      type t = Element.t * props
    end

    open Action
    module Result = Element

    let name = "Draggable"

    let translation freedom x0 y0 x1 y1 =
      match freedom with
      | Free -> x1 -. x0, y1 -. y0
      | X -> x1 -. x0, 0.
      | y -> 0., y1 -. y0


    let compute ~inject ((child, input) : Input.t) (model : Model.t) =
      let handle_mouse_down ({ button; mouseX; mouseY; _ } : Node_events.Mouse_button.t) =
        Event.Many
          [ ( match button with
            | BUTTON_LEFT -> inject (Grab (mouseX, mouseY))
            | _ -> Event.no_op )
          ] in
      let handle_mouse_up ({ button; _ } : Node_events.Mouse_button.t) =
        Event.Many
          ( match button with
          | BUTTON_LEFT ->
            inject Drop
            :: input.on_drop model.bounding_box
            :: (if input.snap_back then [ inject Reset ] else [])
          | BUTTON_RIGHT -> [ inject Reset ]
          | _ -> [ Event.no_op ] ) in
      let handle_mouse_move ({ mouseX = x1; mouseY = y1; _ } : Node_events.Mouse_move.t) =
        Event.Many
          ( match model.start with
          | Some (x0, y0) ->
            let x, y = translation input.freedom x0 y0 x1 y1 in
            [ inject (Drag (x, y)); input.on_drag ~x ~y ]
          | None -> [ Event.no_op ] ) in
      let handle_bounding_box_change bb = Event.Many [ inject (SetBoundingBox bb) ] in
      let trans = Style.(transform [ TranslateX model.x_trans; TranslateY model.y_trans ]) in

      box
        Attr.(
          on_mouse_down handle_mouse_down
          :: on_mouse_up handle_mouse_up
          :: on_mouse_move handle_mouse_move
          :: on_bounding_box_changed handle_bounding_box_change
          :: style (trans :: input.styles)
          :: input.attributes)
        [ child ]


    let apply_action ~inject:_ ~schedule_event:_ _ (model : Model.t) = function
      | Grab (x, y) -> { model with start = Some (x -. model.x_trans, y -. model.y_trans) }
      | Drop -> { model with start = None }
      | Drag (x, y) -> { model with x_trans = x; y_trans = y }
      | Reset -> { model with x_trans = 0.; y_trans = 0. }
      | SetBoundingBox bb -> { model with bounding_box = bb }
  end

  let component = Bonsai.of_module (module T) ~default_model:T.Model.default
end

module Slider = struct
  type props =
    { on_value_changed : float -> Event.t
    ; vertical : bool
    ; min_value : float
    ; max_value : float
    ; init_value : float
    ; slider_length : int
    ; thumb_length : int option
    ; thumb_thickness : int
    ; track_thickness : int
    ; max_track_color : (Color.t[@sexp.opaque])
    ; min_track_color : (Color.t[@sexp.opaque])
    ; thumb_color : (Color.t[@sexp.opaque])
    }
  [@@deriving sexp_of]

  let props
      ?(on_value_changed = fun _ -> Event.no_op)
      ?(vertical = false)
      ?(min_value = 0.)
      ?(max_value = 1.)
      ?(init_value = 0.)
      ?(slider_length = 100)
      ?(thumb_length = Some 15)
      ?(thumb_thickness = 15)
      ?(track_thickness = 5)
      ?(max_track_color = Colors.dark_gray)
      ?(min_track_color = Color.hex "#90f7ff")
      ?(thumb_color = Colors.gray)
      ()
    =
    { on_value_changed
    ; vertical
    ; min_value
    ; max_value
    ; init_value
    ; slider_length
    ; thumb_length
    ; thumb_thickness
    ; track_thickness
    ; max_track_color
    ; min_track_color
    ; thumb_color
    }


  module T = struct
    module Model = struct
      type t =
        { value : float option
        ; bounding_box : (BoundingBox2d.t[@sexp.opaque])
        }
      [@@deriving equal, sexp]

      let default = { value = None; bounding_box = BoundingBox2d.create 0. 0. 1. 1. }
    end

    module Action = struct
      type t =
        | Slide of float
        | SetBoundingBox of (BoundingBox2d.t[@sexp.opaque])
      [@@deriving sexp_of]
    end

    module Input = struct
      type t = Element.t * props
    end

    open Action

    module Result = struct
      (* TODO: What should be here (in addition to the element)? value, set max callback (any other
         params need to be set by scrollview?) *)
      type t = float * (float -> Event.t) * Element.t
    end

    let name = "Slider"
    let compute ~inject ((bar, input) : Input.t) (model : Model.t) = ()

    let apply_action ~inject:_ ~schedule_event:_ _ (model : Model.t) = function
      | Slide v -> { model with value = Some v }
      | SetBoundingBox bb -> { model with bounding_box = bb }
  end

  (* let component = Bonsai.of_module (module T) ~default_model:T.Model.default *)
end

module ScrollView = struct
  type props =
    { speed : float
    ; styles : (Style.t[@sexp.opaque]) list
    ; attributes : Attr.t list
    }
  [@@deriving sexp_of]

  let props ?(speed = 25.) ?(attributes = []) styles = { speed; styles; attributes }

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
        ; width : int
        ; height : int
        ; child_dims : (int * int) Map.M(Int).t
        }
      [@@deriving equal, sexp]

      let default =
        { x_pos = 0.; y_pos = 0.; width = 0; height = 0; child_dims = Map.empty (module Int) }
    end

    module Action = struct
      type t =
        | HorizontalScroll of float
        | VerticalScroll of float
        | Dimensions of int * int
        | ChildDimensions of int * int * int
        | TrimChildren of Set.M(Int).t
      [@@deriving sexp_of]
    end

    module Input = struct
      type t = Element.t Map.M(Int).t * props
    end

    open Action
    module Result = Element

    let name = "ScrollView"

    (* Sum over the major dimension, max over the minor dimension. *)
    let calculate_totals columnar dims =
      let w_fun, h_fun = if columnar then Int.max, ( + ) else ( + ), Int.max in
      let f ~key:_ ~data:(w, h) (w_acc, h_acc) = w_fun w_acc w, h_fun h_acc h in
      dims |> Map.fold ~init:(0, 0) ~f


    let compute ~inject ((children, input) : Input.t) (model : Model.t) =
      let total_width, total_height = calculate_totals (is_columnar input.styles) model.child_dims in
      let diff_width =
        Float.(of_int Int.(total_width - model.width) |> clamp_exn ~min:0. ~max:max_value) in
      let diff_height =
        Float.(of_int Int.(total_height - model.height) |> clamp_exn ~min:0. ~max:max_value) in
      let fudge_height = Float.(if diff_height > 0. then of_int model.height * 0.05 else 0.) in
      let fudge_width = Float.(if diff_width > 0. then of_int model.width * 0.05 else 0.) in

      let handle_wheel ({ shiftKey; deltaY; _ } : Node_events.Mouse_wheel.t) =
        let delta = deltaY *. input.speed in
        let event =
          match Float.(abs delta > 0.), shiftKey with
          | true, false ->
            let y_pos =
              model.y_pos +. delta |> Float.clamp_exn ~min:0. ~max:(diff_height +. fudge_height)
            in
            inject (VerticalScroll y_pos)
          | true, true ->
            let x_pos =
              model.x_pos +. (delta *. horizonal_scroll_multiplier)
              |> Float.clamp_exn ~min:0. ~max:(diff_width +. fudge_width) in
            inject (HorizontalScroll x_pos)
          | false, _ -> Event.no_op in
        Event.Many [ event ] in
      let handle_dimension_change ({ width; height } : Node_events.Dimensions_changed.t) =
        Event.Many
          [ inject (Dimensions (width, height)); inject (TrimChildren (Map.key_set children)) ]
      in
      let trans key =
        Attr.
          [ style
              Style.
                [ transform [ TranslateX (-1. *. model.x_pos); TranslateY (-1. *. model.y_pos) ] ]
          ; on_dimensions_changed (fun { width; height } ->
                Event.Many [ inject (ChildDimensions (key, width, height)) ])
          ] in

      box
        Attr.(
          on_mouse_wheel handle_wheel
          :: on_dimensions_changed handle_dimension_change
          :: style input.styles
          :: input.attributes)
        (Map.mapi ~f:(fun ~key ~data -> box (trans key) [ data ]) children |> Map.data)


    let apply_action ~inject:_ ~schedule_event:_ _ (model : Model.t) = function
      | HorizontalScroll x_pos -> { model with x_pos }
      | VerticalScroll y_pos -> { model with y_pos }
      | Dimensions (width, height) -> { model with width; height }
      | ChildDimensions (key, w, h) ->
        { model with child_dims = Map.set model.child_dims ~key ~data:(w, h) }
      | TrimChildren keys ->
        { model with child_dims = Map.filter_keys model.child_dims ~f:(Set.mem keys) }
  end

  let component = Bonsai.of_module (module T) ~default_model:T.Model.default
end
