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

module Resizable = struct
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
      type resize =
        [ `Scale of float option * float option
        | `Set of int option * int option
        ]
      [@@deriving sexp_of]

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
      let resize r = Event.Many [ inject (Resize r) ] in
      let handle_dimensions_changed ({ width; height } : Node_events.Dimensions_changed.t) =
        Event.Many [ inject (OriginalDimensions (width, height)) ] in
      let changed =
        List.filter_opt
          Style.[ Option.map ~f:width model.set_width; Option.map ~f:height model.set_height ] in
      let s = List.filter_opt [ model.set_width; model.set_height ] in
      Log.perf (sprintf "changed: %s" (List.to_string ~f:Int.to_string s)) (fun () -> ());
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
  (* NOTE: See Set_input_node and Set_text_node and using the node_red callback usage in Text_input
     in order to get the actual nodes that underlie the elements (to get measurements from etc). *)

  (* NOTE: Might consider adding an an attribute to govern whether a box gives up capture when the
     mouse leaves it's area while the mouse button is down. It doesn't feel great when trying to
     move a slider when capture is lost. UPDATE: Tried to prevent capture loss by removing it from
     the clickable_box on leave callbacks and was unsuccessful. What would actually need to change? *)
  type bounds =
    { x_min : float option
    ; y_min : float option
    ; x_max : float option
    ; y_max : float option
    }
  [@@deriving sexp_of]

  type props =
    { styles : (Style.t[@sexp.opaque]) list
    ; attributes : Attr.t list
    ; snap_back : bool
    ; bounds : bounds
    ; on_drag : x:float -> y:float -> Event.t
    ; on_drop : BoundingBox2d.t -> Event.t
    }
  [@@deriving sexp_of]

  let props
      ?(attributes = [])
      ?(snap_back = false)
      ?(bounds = { x_min = None; y_min = None; x_max = None; y_max = None })
      ?(on_drag = fun ~x:_ ~y:_ -> Event.no_op)
      ?(on_drop = fun _ -> Event.no_op)
      styles
    =
    { styles; attributes; snap_back; bounds; on_drag; on_drop }


  module T = struct
    module Model = struct
      type t =
        { start : (float * float) option
        ; x_trans : float
        ; y_trans : float (* ; set_w : int option
                           * ; set_h : int option *)
        ; bounding_box : (BoundingBox2d.t[@sexp.opaque])
        }
      [@@deriving equal, sexp]

      let default =
        { start = None
        ; x_trans = 0.
        ; y_trans = 0. (* ; set_w = None
                        * ; set_h = None *)
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
      (* | SetDims of int * int *)
      [@@deriving sexp_of]
    end

    module Input = struct
      type t = Element.t * props
    end

    open Action

    module Result = struct
      type t = BoundingBox2d.t * Element.t
    end

    let name = "Draggable"

    let apply_bounds ?(min = -.Float.max_value) ?(max = Float.max_value) v =
      Float.clamp_exn v ~min ~max


    let translation (bounds : bounds) x0 y0 x1 y1 =
      ( apply_bounds ?min:bounds.x_min ?max:bounds.x_max (x1 -. x0)
      , apply_bounds ?min:bounds.y_min ?max:bounds.y_max (y1 -. y0) )


    let compute ~inject ((child, props) : Input.t) (model : Model.t) =
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
            :: props.on_drop model.bounding_box
            :: (if props.snap_back then [ inject Reset ] else [])
          | BUTTON_RIGHT -> [ inject Reset ]
          | _ -> [ Event.no_op ] ) in
      let handle_mouse_move ({ mouseX = x1; mouseY = y1; _ } : Node_events.Mouse_move.t) =
        Event.Many
          ( match model.start with
          | Some (x0, y0) ->
            let x, y = translation props.bounds x0 y0 x1 y1 in
            [ inject (Drag (x, y)); props.on_drag ~x ~y ]
          | None -> [ Event.no_op ] ) in
      let handle_bounding_box_change bb = Event.Many [ inject (SetBoundingBox bb) ] in
      let trans = Style.(transform [ TranslateX model.x_trans; TranslateY model.y_trans ]) in

      (* let set_dims w h = Event.Many [ inject (SetDims (w, h)) ] in *)
      (* let changed =
       *   List.filter_opt Style.[ Option.map ~f:width model.set_w; Option.map ~f:height model.set_h ]
       * in *)
      let element =
        box
          Attr.(
            (* :: style ((trans :: props.styles) @ changed) *)
            on_mouse_down handle_mouse_down
            :: on_mouse_up handle_mouse_up
            :: on_mouse_move handle_mouse_move
            :: on_bounding_box_changed handle_bounding_box_change
            :: style (trans :: props.styles)
            :: props.attributes)
          [ child ] in
      (* model.bounding_box, set_dims, element *)
      model.bounding_box, element


    let apply_action ~inject:_ ~schedule_event:_ _ (model : Model.t) = function
      | Grab (x, y) -> { model with start = Some (x -. model.x_trans, y -. model.y_trans) }
      | Drop -> { model with start = None }
      | Drag (x, y) -> { model with x_trans = x; y_trans = y }
      | Reset -> { model with x_trans = 0.; y_trans = 0. }
      | SetBoundingBox bb -> { model with bounding_box = bb }

    (* | SetDims (w, h) -> { model with set_w = Some w; set_h = Some h } *)
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
    ; track_thickness : int
    ; track_color : (Color.t[@sexp.opaque])
    ; thumb : (Draggable.props[@sexp.opaque])
    }
  [@@deriving sexp_of]

  (* TODO: Make length fully optional? Then go off of current dimensions with flex on if not set? *)
  let props
      ?(on_value_changed = fun _ -> Event.no_op)
      ?(vertical = true)
      ?(min_value = 0.)
      ?(max_value = 1.)
      ?(init_value = 0.)
      ?(slider_length = 100)
      ?(thumb_length = 15)
      ?(thumb_thickness = 15)
      ?(track_thickness = 15)
      ?(track_color = Colors.dark_gray)
      ?(thumb_color = Colors.gray)
      ()
    =
    let bounds =
      let open Draggable in
      if vertical
      then
        { x_min = Some 0.
        ; y_min = Some 0.
        ; x_max = Some 0.
        ; y_max = Some (Float.of_int (slider_length - thumb_length))
        }
      else
        { x_min = Some 0.
        ; y_min = Some 0.
        ; x_max = Some (Float.of_int (slider_length - thumb_length))
        ; y_max = Some 0.
        } in
    let thumb =
      Draggable.(
        props
          ~snap_back:false
          ~bounds
          Style.
            [ width (if vertical then thumb_thickness else thumb_length)
            ; height (if vertical then thumb_length else thumb_thickness)
            ; background_color thumb_color
            ]) in
    { on_value_changed
    ; vertical
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
        { value : float option
        ; bounding_box : (BoundingBox2d.t[@sexp.opaque]) option
        }
      [@@deriving equal, sexp]

      let default = { value = None; bounding_box = None }
    end

    module Action = struct
      type t = SetBoundingBox of (BoundingBox2d.t[@sexp.opaque]) [@@deriving sexp_of]
    end

    module Input = struct
      type t = BoundingBox2d.t * Element.t * props
    end

    open Action

    module Result = struct
      type t = float * Element.t
    end

    let name = "Slider"

    let compute ~inject ((bar_bb, bar, props) : Input.t) (model : Model.t) =
      let handle_bounding_box_change bb = Event.Many [ inject (SetBoundingBox bb) ] in
      let value =
        Option.value_map model.bounding_box ~default:props.init_value ~f:(fun bb ->
            let s_l, s_t, s_r, s_b = BoundingBox2d.get_bounds bb in
            let b_l, b_t, b_r, b_b = BoundingBox2d.get_bounds bar_bb in
            ( if props.vertical
            then (s_t -. b_t) /. (s_t -. s_b -. b_t +. b_b +. 0.000001)
            else (s_l -. b_l) /. (s_l -. s_r -. b_l +. b_r +. 0.000001) )
            |> fun v -> (v *. (props.max_value -. props.min_value)) +. props.min_value) in
      let styles =
        Style.
          [ background_color props.track_color
          ; width (if props.vertical then props.track_thickness else props.slider_length)
            (* ; height (if props.vertical then props.slider_length else props.track_thickness) *)
            (* ; height (if props.vertical then 0 else props.track_thickness) *)
          ; flex_direction `Column
          ; flex_grow 1
          ] in
      let element =
        box Attr.[ on_bounding_box_changed handle_bounding_box_change; style styles ] [ bar ] in
      value, element


    let apply_action ~inject:_ ~schedule_event:_ _ (model : Model.t) = function
      | SetBoundingBox bb -> { model with bounding_box = Some bb }
  end

  (* let component_v1 =
   *   let component = Bonsai.of_module (module T) ~default_model:T.Model.default in
   *   let pure = Bonsai.pure ~f:(fun (props : props) -> text [] "", props.thumb) in
   *   Bonsai.Arrow.partial_compose_first
   *     (pure >>> Draggable.component >>| fun (bb, draggable) -> (bb, draggable), ())
   *     ((fun (props, (bb, draggable)) -> bb, draggable, props) @>> component)
   *   >>| fun ((), (value, slider)) -> value, slider *)

  (* I think this version is a bit cleaner. *)
  let component =
    let component = Bonsai.of_module (module T) ~default_model:T.Model.default in
    Bonsai.Arrow.pipe
      (Bonsai.pure ~f:(fun (props : props) -> text [] "", props.thumb) >>> Draggable.component)
      ~via:(fun props (bb, draggable) -> bb, draggable, props)
      ~into:component
      ~finalize:(fun _ _ (value, slider) -> value, slider)
end

module ScrollView = struct
  type props =
    { speed : float
    ; styles : (Style.t[@sexp.opaque]) list
    ; attributes : Attr.t list
    ; x_slider : Slider.props
    ; y_slider : Slider.props
    }
  [@@deriving sexp_of]

  let props ?(speed = 25.) ?(attributes = []) ?track_color ?thumb_color styles =
    let common = Slider.props ?track_color ?thumb_color in
    let x_slider = common ~vertical:false () in
    let y_slider = common ~vertical:true () in
    { speed; styles; attributes; x_slider; y_slider }


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
        ; scroll_width : float
        ; scroll_height : float
        ; child_count : int
        ; child_dims : (int * int) Map.M(Int).t
        }
      [@@deriving equal, sexp]

      let default =
        { x_pos = 0.
        ; y_pos = 0.
        ; width = 0
        ; height = 0
        ; scroll_width = 0.
        ; scroll_height = 0.
        ; child_count = 0
        ; child_dims = Map.empty (module Int)
        }
    end

    module Action = struct
      type t =
        | Scroll of float * float
        | Dimensions of int * int
        | Scrollable of float * float
        | Count of int
        | ChildDimensions of int * int * int
        | TrimChildren of Set.M(Int).t
      [@@deriving sexp_of]
    end

    module Input = struct
      type control =
        [ `Uncontrolled
        | `Controlled of float option * float option
        ]

      type slider =
        { ele : Element.t
        ; resize :
            [ `Scale of float option * float option | `Set of int option * int option ] -> Event.t
        }

      type sliders = { (* x : slider *)
                       y : slider }

      type t = control * sliders * Element.t Map.M(Int).t * props
    end

    open Action
    module Result = Element

    let name = "ScrollView"

    (* Sum over the major dimension, max over the minor dimension. *)
    let calculate_totals columnar dims =
      let w_fun, h_fun = if columnar then Int.max, ( + ) else ( + ), Int.max in
      let f ~key:_ ~data:(w, h) (w_acc, h_acc) = w_fun w_acc w, h_fun h_acc h in
      dims |> Map.fold ~init:(0, 0) ~f


    let excess total limit = Float.(of_int Int.(total - limit) |> clamp_exn ~min:0. ~max:max_value)
    let fudge ex limit = Float.(ex + if ex > 0. then of_int limit * 0.05 else 0.)

    let scrollable columnar width height child_dims =
      let children_width, children_height = calculate_totals columnar child_dims in
      let w = fudge (excess children_width width) width in
      let h = fudge (excess children_height height) height in
      w, h


    let compute ~inject ((control, sliders, children, props) : Input.t) (model : Model.t) =
      let count = Map.length children in
      if count <> model.child_count
      then
        Event.Many
          [ inject (Count count)
          ; ( if count < model.child_count
            then inject (TrimChildren (Map.key_set children))
            else Event.no_op )
          ]
        |> Event.Expert.handle;

      let scroll_width, scroll_height =
        scrollable (is_columnar props.styles) model.width model.height model.child_dims in

      if Float.(model.scroll_width <> scroll_width || model.scroll_height <> scroll_height)
      then
        Event.Many
          [ inject (Scrollable (scroll_width, scroll_height))
          ; sliders.y.resize (`Set (None, Some (Int.of_float scroll_height)))
          ]
        |> Event.Expert.handle;

      let handle_wheel ({ shiftKey; deltaY; _ } : Node_events.Mouse_wheel.t) =
        let x_lock, y_lock =
          match control with
          | `Controlled (x, y) -> Option.(is_some x, is_some y)
          | `Uncontrolled -> false, false in
        let event =
          let delta = deltaY *. props.speed in
          match Float.(abs delta > 0.), shiftKey with
          | true, false when not y_lock ->
            let y_pos = model.y_pos +. delta |> Float.clamp_exn ~min:0. ~max:scroll_height in
            inject (Scroll (model.x_pos, y_pos))
          | true, true when not x_lock ->
            let x_pos =
              model.x_pos +. (delta *. horizonal_scroll_multiplier)
              |> Float.clamp_exn ~min:0. ~max:scroll_width in
            inject (Scroll (x_pos, model.y_pos))
          | _ -> Event.no_op in
        Event.Many [ event ] in
      let handle_dimension_change ({ width; height } : Node_events.Dimensions_changed.t) =
        Event.Many [ inject (Dimensions (width, height)) ] in

      let trans_x, trans_y =
        match control with
        | `Controlled (x_opt, y_opt) ->
          let x = Option.value_map ~default:model.x_pos ~f:(( *. ) scroll_width) x_opt in
          let y = Option.value_map ~default:model.y_pos ~f:(( *. ) scroll_height) y_opt in
          x, y
        | _ -> model.x_pos, model.y_pos in
      let trans key =
        Attr.
          [ style Style.[ transform [ TranslateX (-1. *. trans_x); TranslateY (-1. *. trans_y) ] ]
          ; on_dimensions_changed (fun { width; height } ->
                Event.Many [ inject (ChildDimensions (key, width, height)) ])
          ] in

      let view =
        box
          Attr.(
            on_mouse_wheel handle_wheel
            :: on_dimensions_changed handle_dimension_change
            :: style props.styles
            :: props.attributes)
          (Map.mapi ~f:(fun ~key ~data -> box (trans key) [ data ]) children |> Map.data) in

      (* List.rev to overwrite styles *)
      box
        Attr.[ style Style.(flex_direction `Row :: props.styles |> List.rev) ]
        [ view; sliders.y.ele ]


    let apply_action ~inject:_ ~schedule_event:_ _ (model : Model.t) = function
      | Scroll (x_pos, y_pos) -> { model with x_pos; y_pos }
      | Dimensions (width, height) -> { model with width; height }
      | Count n -> { model with child_count = n }
      | ChildDimensions (key, w, h) ->
        { model with child_dims = Map.set model.child_dims ~key ~data:(w, h) }
      | TrimChildren keys ->
        { model with child_dims = Map.filter_keys model.child_dims ~f:(Set.mem keys) }
      | Scrollable (w, h) -> { model with scroll_width = w; scroll_height = h }
  end

  (* let component = Bonsai.of_module (module T) ~default_model:T.Model.default *)

  let slider_props ax props =
    match ax with
    | `X -> props.x_slider
    | `Y -> props.y_slider


  (* NOTE: I actually don't know yet whether the resize component is workable for this or whether
     the resizing should just live in slider, since I think I am still over-writing the size info
     when building the slider component anyway. So, need to test that out as well as thinking about
     how to make the slider component more flexible in general. Not the end of the world, I've got
     most of the pieces that would be rearranged anyway. Having just tried again, I'm not sure that
     it is in the cards. In thumb props, I can indicate how the thumb should be controlled along
     with the track length via optional args (without defaults). Fixed and dynamic props profiles
     could make things friendly as well.*)
  let with_sliders =
    let component = Bonsai.of_module (module T) ~default_model:T.Model.default in
    let thumb =
      Bonsai.Arrow.pipe
        ( Bonsai.pure ~f:(fun (children, (props : props)) ->
              let resize_props =
                Resizable.props
                  ~attributes:props.y_slider.thumb.attributes
                  (props.y_slider.thumb.styles @ Style.[ flex_grow 1 ]) in
              text Attr.[ style Style.[ flex_grow 1 ] ] "", resize_props)
        >>> Resizable.component )
        ~via:(fun (_, props) (resize, thumb) -> thumb, props.y_slider.thumb)
        ~into:Draggable.component
        ~finalize:(fun props (resize, thumb) (bb, draggable) -> bb, resize, draggable) in
    let slider =
      Bonsai.Arrow.pipe
        thumb
        ~via:(fun (children, props) (bb, resize, draggable) -> props.y_slider)
        ~into:Slider.component
        ~finalize:(fun (children, props) (bb, resize, draggable) (value, slider) ->
          let sliders = T.Input.{ y = { ele = slider; resize } } in
          `Controlled (None, Some value), sliders, children, props) in
    slider >>> component
end
