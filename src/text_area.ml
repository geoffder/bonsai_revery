open Core_kernel
open Import
open Bonsai.Infix
open Components
module Attr = Attributes

(* FIXME: Would like to wrap long words, rather than extend out of the container and
 * require scrolling, but with how wrapping works in revery, I would need to change a lot
 * of fundamentals here (might in future, to be more like markdown.re to enable better
 * selection highlighting, and other inline formatting etc). If I want it bad enough,
 * consider creating a new wrapping mode in a Revery PR, similar to WrapIgnoreWhiteSpace
 * but with non-hyphenated word breaking when a single word exceeds the available space.
 *
 * Possible workaround might be to mark row beginnings in OffsetMap accordingly, then
 * use newline_hack to insert the \n, haven't considered enough yet though. *)

type props =
  { autofocus : bool
  ; text_color : Color.t
  ; cursor_color : Color.t
  ; select_color : Color.t
  ; placeholder : string
  ; placeholder_color : Color.t
  ; default_value : string option
  ; on_key_down : Node_events.Keyboard.t -> string -> (string -> Event.t) -> Event.t
  ; max_height : int
  ; attributes : Attr.t list
  }

let props
    ?(autofocus = false)
    ?(text_color = Colors.black)
    ?(cursor_color = Revery.UI.Components.Input.Styles.defaultCursorColor)
    ?(select_color = Revery.Colors.blueViolet)
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
  ; select_color
  ; placeholder
  ; placeholder_color
  ; default_value
  ; on_key_down
  ; max_height
  ; attributes
  }


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


let is_space = Char.equal ' '
let is_newline = Char.equal '\n'
let zero_space = "\xe2\x80\x8b"

let next_non_whitespace str from =
  String.lfindi ~pos:from ~f:(fun _ c -> not (is_space c || is_newline c)) str


module OffsetMap = struct
  module Row = struct
    type t =
      { start : int
      ; y_offset : float
      ; x_offsets : float list
      }

    let equal a b =
      a.start = b.start
      && Float.equal a.y_offset b.y_offset
      && List.equal Float.equal a.x_offsets b.x_offsets


    let to_string { start; y_offset; x_offsets } =
      let xs = List.to_string ~f:(sprintf "%.2f") x_offsets in
      sprintf "start = %i; y_offset = %.2f; x_offsets = %s" start y_offset xs


    let nearest_index t x_offset =
      let rec loop i last_offset = function
        | [] -> i
        | h :: t ->
          if Float.(h > x_offset)
          then if Float.(h - x_offset < x_offset - last_offset) then i + 1 else i
          else loop (i + 1) h t in
      loop 0 0. t.x_offsets + t.start
  end

  type t = Row.t Map.M(Int).t

  let to_string m =
    Map.fold m ~init:"\n" ~f:(fun ~key ~data acc ->
        sprintf "%srow %i -> %s\n" acc key (Row.to_string data))


  (* TODO: refactor OffsetMap creation such that a position in text can be specified
   * from which the update will occur. From the beginning of the containing row to
   * the end of the text will have to be updated, but it will will represent large
   * computational savings *)

  let make font_info line_height margin text =
    let len = String.length text in
    let measure_width = measure_text_width font_info in
    let add_row (t : t) row start x_offsets =
      Map.add_exn t ~key:row ~data:{ start; y_offset = Float.of_int row *. line_height; x_offsets }
    in
    let f (pos, row, row_start, offsets, t) c =
      (* zero_width character used in newline_hack to prevent leading space collapse *)
      let pad = if is_space text.[row_start] then zero_space else "" in
      let width = measure_width (pad ^ String.slice text row_start pos) in
      let next = pos + 1 in
      match c with
      | ' '
      (* Lookahead to next whitespace to see if upcoming word overflows. *)
        when next < len
             &&
             let word_start = next_non_whitespace text next in
             let lookahead =
               match
                 ( Option.bind word_start ~f:(fun n -> String.index_from text n ' ')
                 , String.index_from text next '\n' )
               with
               | Some next_space, Some next_break -> Int.min next_space next_break
               | Some next_space, None -> next_space
               | None, Some next_break -> next_break
               | _ -> len in
             Float.( > ) (measure_width (pad ^ String.slice text row_start lookahead)) margin ->
        next, row + 1, pos, [], add_row t row row_start (List.rev offsets)
      | '\n' -> next, row + 1, pos, [], add_row t row row_start (List.rev offsets)
      | c -> next, row, row_start, width :: offsets, t in
    let _, row, row_start, offsets, t =
      String.fold ~init:(1, 0, 0, [], Map.empty (module Int)) ~f text in
    add_row t row row_start (List.rev offsets)


  let nearest_index (t : t) x_offset y_offset =
    let x_offset = Float.max 0. x_offset in
    let rec find_row last_row i =
      match Map.find t i, last_row with
      | (Some row as current), Some (last : Row.t) ->
        if Float.(row.y_offset > y_offset)
        then
          if Float.(row.y_offset - y_offset < y_offset - last.y_offset) then current else last_row
        else find_row current (i + 1)
      | (Some row as current), None ->
        if Float.(row.y_offset > y_offset) then current else find_row current (i + 1)
      | None, Some _ -> last_row
      | None, None -> None in
    Option.map (find_row None 0) ~f:(fun row -> Row.nearest_index row x_offset)


  let find_index t index =
    let index = index - 1 in
    let f i (r : Row.t) =
      if index < r.start + List.length r.x_offsets
      then (
        let x = Option.value ~default:0. (List.nth r.x_offsets (index - r.start)) in
        Some (i, x, r.y_offset) )
      else None in
    Option.value ~default:(0, 0., 0.) (List.find_mapi ~f (Map.data t))


  let max_x_offset t =
    let f ~key:_ ~data:({ x_offsets; _ } : Row.t) m =
      Float.max m (List.fold ~init:0. ~f:Float.max x_offsets) in
    Map.fold t ~init:0. ~f


  let max_y_offset (t : t) =
    Option.value_map (Map.max_elt t) ~default:0. ~f:(fun (_, row) -> row.y_offset)


  let row_widths (t : t) =
    List.map (Map.data t) ~f:(fun { x_offsets; _ } -> List.fold ~init:0. ~f:Float.max x_offsets)
end

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
      ; select_start : int option
      ; offsets : (OffsetMap.t[@sexp.opaque] [@equal.ignore])
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
      ; select_start = None
      ; offsets = Map.empty (module Int)
      }
  end

  module Action = struct
    type t =
      | Focus
      | Blur
      | Select of int
      | Unselect
      | Text_input of string * int
      | Set_value of string
      | UpdateOffsets of bool
      | Reposition of int * float * float * float * float
      | Set_text_node of (UI.node[@sexp.opaque])
      | Set_input_node of (UI.node[@sexp.opaque])
    [@@deriving sexp_of]
  end

  open Action

  module Result = struct
    type t = string * (string -> Event.t) * Element.t
  end

  let name = "InputArea"

  let styles =
    Style.
      [ cursor `Text
      ; flex_direction `Row
      ; align_items `Center
      ; margin_left 10
      ; margin_right 10
      ; flex_grow 1
      ; justify_content `FlexStart
      ]


  let default_text_spec = { Attr.KindSpec.Text.default with size = 18. }
  let default_kind = Attr.KindSpec.(TextNode default_text_spec)

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


  let vertical_nav ~up (m : OffsetMap.t) start_position =
    let row, target_x, _ = OffsetMap.find_index m start_position in
    let target_row = if up then row - 1 else row + 1 in
    Option.value_map (Map.find m target_row) ~default:start_position ~f:(fun row ->
        OffsetMap.Row.nearest_index row target_x)


  (* Workaround for Revery text wrapping not working exactly as I'd like.
   * - spaces inserted following newlines on empty rows to prevent collapse
   * - zero width unicodes inserted before leading spaces
   * - spaces which triggered a wrap are replaced by newlines *)
  let newline_hack offsets text =
    let len = String.length text in
    let f acc ({ start; x_offsets; _ } : OffsetMap.Row.t) =
      let before = Str.string_before acc start in
      let after = Str.string_after acc start in
      let spacer =
        if List.length x_offsets = 0
        then " "
        else if start < len && is_space text.[start]
        then zero_space
        else "" in
      if start > 1 && is_space text.[start - 1]
      then String.drop_suffix before 1 ^ "\n" ^ spacer ^ after
      else before ^ spacer ^ after in
    List.fold ~f ~init:text (List.rev (Map.data offsets))


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
      let rec loop i = if i = len || Set.mem end_chars str.[sub_len - i] then i else loop (i + 1) in
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


  let remove_between text p1 p2 =
    let first, last = if p1 > p2 then p2, p1 else p1, p2 in
    Str.string_before text first ^ Str.string_after text last, first


  let select_parts text p1 p2 =
    let first, last = if p1 > p2 then p2, p1 else p1, p2 in
    let before = Str.string_before text first in
    let selected = String.slice text first last in
    let after = Str.string_after text last in
    before, selected, after


  let copy_selected text p1 p2 =
    let first, last = if p1 > p2 then p2, p1 else p1, p2 in
    Sdl2.Clipboard.setText (String.slice text first last)


  let compute ~inject ((cursor_on, props) : Input.t) (model : Model.t) =
    let open Revery.UI.Components.Input in
    let font_info = get_font_info props.attributes in
    let value = Option.first_some model.value props.default_value |> Option.value ~default:"" in
    let show_placeholder = String.equal value "" in
    let cursor_position = model.cursor_position in

    let measure_text_width text = measure_text_width font_info text in
    let set_value value = inject (Set_value value) in
    let update value cursor_position = inject (Text_input (value, cursor_position)) in

    let selection_event shift new_position =
      if shift
      then
        if Option.is_none model.select_start && new_position <> cursor_position
        then [ inject (Select cursor_position) ]
        else []
      else [ inject Unselect ] in

    let paste value cursor_position =
      let value, cursor_position =
        match model.select_start with
        | Some pos -> remove_between value pos cursor_position
        | None -> value, cursor_position in
      match Sdl2.Clipboard.getText () with
      | None -> Event.no_op
      | Some data ->
        let value, cursor_position = insertString value data cursor_position in
        update value cursor_position in

    let handle_text_input (event : Node_events.Text_input.t) =
      let value, cursor_position =
        match model.select_start with
        | Some pos -> remove_between value pos cursor_position
        | None -> value, cursor_position in
      let value, cursor_position = insertString value event.text cursor_position in
      update value cursor_position in

    let handle_key_down (keyboard_event : Node_events.Keyboard.t) =
      let event =
        match keyboard_event.key with
        | Left ->
          let new_position =
            if keyboard_event.ctrl
            then (
              let before, _ = getStringParts cursor_position value in
              cursor_position - chars_to_previous_word_end before )
            else getSafeStringBounds value cursor_position (-1) in
          Event.Many (update value new_position :: selection_event keyboard_event.shift new_position)
        | Right ->
          let new_position =
            if keyboard_event.ctrl
            then (
              let _, after = getStringParts cursor_position value in
              cursor_position + chars_to_next_word_end after )
            else getSafeStringBounds value cursor_position 1 in
          Event.Many (update value new_position :: selection_event keyboard_event.shift new_position)
        | Up ->
          let new_position = vertical_nav ~up:true model.offsets cursor_position in
          Event.Many (update value new_position :: selection_event keyboard_event.shift new_position)
        | Down ->
          let new_position = vertical_nav ~up:false model.offsets cursor_position in
          Event.Many (update value new_position :: selection_event keyboard_event.shift new_position)
        | Delete ->
          let value, cursor_position =
            match model.select_start with
            | Some pos -> remove_between value pos cursor_position
            | None ->
              if keyboard_event.ctrl
              then remove_word_after value cursor_position
              else removeCharacterAfter value cursor_position in
          update value cursor_position
        | Backspace ->
          let value, cursor_position =
            match model.select_start with
            | Some pos -> remove_between value pos cursor_position
            | None ->
              if keyboard_event.ctrl
              then remove_word_before value cursor_position
              else removeCharacterBefore value cursor_position in
          update value cursor_position
        | V when keyboard_event.ctrl -> paste value cursor_position
        | C when keyboard_event.ctrl ->
          Option.iter model.select_start ~f:(fun pos -> copy_selected value pos cursor_position);
          Event.no_op
        | Return when keyboard_event.shift ->
          let value, cursor_position =
            match model.select_start with
            | Some pos -> remove_between value pos cursor_position
            | None -> value, cursor_position in
          let value, cursor_position = insertString value "\n" cursor_position in
          update value cursor_position
        | Escape ->
          UI.Focus.loseFocus ();
          Event.no_op
        | _ -> Event.no_op in
      Event.Many [ event; props.on_key_down keyboard_event value set_value ] in

    let handle_mouse_down ({ shiftKey; mouseX; mouseY; _ } : Node_events.Mouse_button.t) =
      match model.text_node, Option.bind model.text_node ~f:(fun n -> n#getParent ()) with
      | Some node, Some parent ->
        let container : UI.Dimensions.t = parent#measurements () in
        let line_height = get_line_height font_info node in
        let margin = Float.of_int container.width in
        let box_height = Float.of_int container.height in
        let scene_offsets = (node#getSceneOffsets () : UI.Offset.t) in
        let x_text_offset = mouseX -. Float.of_int scene_offsets.left in
        let y_text_offset = mouseY -. Float.of_int scene_offsets.top +. model.y_scroll in
        let new_position =
          OffsetMap.nearest_index model.offsets x_text_offset y_text_offset
          |> Option.value ~default:(String.length value) in
        let max_x_offset = OffsetMap.max_x_offset model.offsets in
        let max_y_offset = OffsetMap.max_y_offset model.offsets in
        let text_height = max_y_offset +. line_height in

        let handle_mouse_move
            (pos, x_scroll, y_scroll, last) ({ mouseX; mouseY; _ } : Node_events.Mouse_move.t)
          =
          let now = Time.now () in
          if Float.(Time.Span.to_ms (Time.diff now last) > 16.)
          then (
            let _, x_offset, y_offset = OffsetMap.find_index model.offsets pos in
            let x_scroll = horizontal_scroll margin max_x_offset x_offset x_scroll in
            let y_scroll = vertical_scroll box_height text_height line_height y_offset y_scroll in
            let x_text_offset = mouseX -. Float.of_int scene_offsets.left +. x_scroll in
            let y_text_offset = mouseY -. Float.of_int scene_offsets.top +. y_scroll in
            let new_pos =
              OffsetMap.nearest_index model.offsets x_text_offset y_text_offset
              |> Option.value ~default:(String.length value) in
            let repos = inject (Reposition (new_pos, x_offset, y_offset, x_scroll, y_scroll)) in
            repos, Some (new_pos, x_scroll, y_scroll, now) )
          else Event.no_op, Some (pos, x_scroll, y_scroll, last) in

        Option.iter model.input_node ~f:UI.Focus.focus;
        mouse_capture
          ~on_mouse_move:handle_mouse_move
          ~on_mouse_up:(fun _ _ -> Event.no_op, None)
          (new_position, model.x_scroll, model.y_scroll, Time.now ());
        Event.Many
          [ update value new_position
          ; (if shiftKey then inject (Select cursor_position) else inject (Select new_position))
          ]
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

    let stripe ~len ~h ~x ~y =
      let position_style = Style.[ position `Absolute; transform [ TranslateX x; TranslateY y ] ] in
      box
        Attr.[ style position_style ]
        [ opacity
            ~opacity:0.5
            [ box
                Attr.
                  [ style
                      Style.
                        [ width (Int.of_float len)
                        ; height (Int.of_float h)
                        ; background_color props.select_color
                        ]
                  ]
                []
            ]
        ] in

    let highlights =
      match model.select_start, Option.bind model.text_node ~f:(fun n -> n#getParent ()) with
      | Some start, Some node ->
        let first = Int.min cursor_position start in
        let last = Int.max cursor_position start in
        let line_height = get_line_height font_info node in
        let start_line, first_x, first_y = OffsetMap.find_index model.offsets first in
        let _, last_x, last_y = OffsetMap.find_index model.offsets last in
        let widths = OffsetMap.row_widths model.offsets in
        ( match Int.of_float ((last_y -. first_y) /. line_height) with
        | 0 ->
          [ stripe
              ~len:(last_x -. first_x)
              ~h:line_height
              ~x:(first_x -. model.x_scroll)
              ~y:(first_y -. model.y_scroll)
          ]
        | n ->
          stripe
            ~len:(List.nth_exn widths start_line -. first_x)
            ~h:line_height
            ~x:(first_x -. model.x_scroll)
            ~y:(first_y -. model.y_scroll)
          :: List.foldi
               ~init:[ stripe ~len:last_x ~h:line_height ~x:0. ~y:(last_y -. model.y_scroll) ]
               ~f:(fun i stripes w ->
                 stripe
                   ~len:w
                   ~h:line_height
                   ~x:0.
                   ~y:(first_y -. model.y_scroll +. (Float.of_int (i + 1) *. line_height))
                 :: stripes)
               (List.slice widths (start_line + 1) (start_line + n)) )
      | _ -> [] in

    let attributes =
      Attr.(
        node_ref (fun node ->
            if props.autofocus then UI.Focus.focus node;
            inject (Set_input_node node))
        :: on_mouse_down handle_mouse_down
        :: on_key_down handle_key_down
        :: on_text_input handle_text_input
        :: on_focus (inject Focus)
        :: on_blur (inject Blur)
        :: style Style.(max_height props.max_height :: styles)
        :: props.attributes) in

    let view =
      box
        attributes
        ( box
            Attr.
              [ style Style.[ flex_grow 1; margin_right (Int.of_float @@ measure_text_width "_") ]
              ; on_dimensions_changed (fun d -> inject (UpdateOffsets false))
              ]
            [ text
                Attr.
                  [ node_ref (fun node -> inject (Set_text_node node))
                  ; style
                      Style.
                        [ color
                            (if show_placeholder then props.placeholder_color else props.text_color)
                        ; justify_content `FlexStart
                        ; align_items `Center
                        ; text_wrap WrapIgnoreWhitespace
                        ; transform [ TranslateX (-.model.x_scroll); TranslateY (-.model.y_scroll) ]
                        ; min_height (Int.of_float (measure_text_height font_info))
                        ]
                  ; kind (TextNode font_info)
                  ]
                (if show_placeholder then props.placeholder else newline_hack model.offsets value)
            ]
        :: cursor
        :: highlights ) in

    value, set_value, view


  let apply_action ~inject ~schedule_event ((_, props) : Input.t) (model : Model.t) = function
    | Focus ->
      Sdl2.TextInput.start ();
      { model with focused = true }
    | Blur ->
      Sdl2.TextInput.stop ();
      { model with focused = false }
    | Text_input (value, cursor_position) ->
      let old_value =
        Option.first_some model.value props.default_value |> Option.value ~default:"" in
      let value_changed = not (String.equal value old_value) in
      let select_start =
        match model.select_start with
        | Some _ when value_changed ->
          Revery_UI.Mouse.releaseCapture ();
          None
        | Some _ -> model.select_start
        | None -> None in
      schedule_event (inject (UpdateOffsets true));
      { model with value = Some value; cursor_position; select_start }
    | Set_value value ->
      Revery_UI.Mouse.releaseCapture ();
      let cursor_position = min model.cursor_position (String.length value) in
      schedule_event (inject (UpdateOffsets true));
      { model with value = Some value; cursor_position; select_start = None }
    | UpdateOffsets value_changed ->
      ( match Option.bind model.text_node ~f:(fun n -> n#getParent ()) with
      | Some node ->
        let value = Option.value ~default:"" model.value in
        let cursor_position = min model.cursor_position (String.length value) in
        let font_info = get_font_info props.attributes in
        let container : UI.Dimensions.t = node#measurements () in
        let margin = Float.(of_int container.width) in
        let line_height = get_line_height font_info node in
        let offsets =
          if value_changed then OffsetMap.make font_info line_height margin value else model.offsets
        in
        let _, x_offset, y_offset = OffsetMap.find_index offsets cursor_position in
        let x_scroll =
          horizontal_scroll margin (OffsetMap.max_x_offset offsets) x_offset model.x_scroll in
        let y_scroll =
          vertical_scroll
            (Float.of_int container.height)
            (OffsetMap.max_y_offset offsets +. line_height)
            line_height
            y_offset
            model.y_scroll in
        { model with x_offset; y_offset; x_scroll; y_scroll; offsets }
      | None -> model )
    | Reposition (cursor_position, x_offset, y_offset, x_scroll, y_scroll) ->
      { model with cursor_position; x_offset; y_offset; x_scroll; y_scroll }
    | Set_text_node node -> { model with text_node = Some node }
    | Set_input_node node -> { model with input_node = Some node }
    | Select position -> { model with select_start = Some position }
    | Unselect ->
      Revery_UI.Mouse.releaseCapture ();
      { model with select_start = None }
end

let component =
  let cursor_on =
    Bonsai.With_incr.of_incr (Incr.Clock.watch_now Incr.clock)
    >>| fun time -> Int.rem (Time_ns.to_span_since_epoch time |> Time_ns.Span.to_int_ms) 1000 > 500
  in

  let component = Bonsai.of_module (module T) ~default_model:T.Model.default in

  let cutoff =
    Bonsai.With_incr.value_cutoff
      ~cutoff:
        (Incr.Cutoff.create
           (fun ~old_value:(old_timer, old_input) ~new_value:(new_timer, new_input) ->
             Bool.equal old_timer new_timer && phys_equal old_input new_input)) in

  ignore @>> cursor_on |> Bonsai.Arrow.extend_first >>> cutoff >>> component

(* NOTE: On trying to see what would happen if I get this component to a ScrollView,
 * I am met with a "node with too large height" exception on the Revery side, so
 * something about this causes the tree to grow out of control. *)
(* let scroll_props =
 *   ScrollView.props
 *     ~track_color:Colors.aqua
 *     ~thumb_color:(Color.hex "#9D77D1")
 *     Style.[ flex_direction `Column; flex_grow 1; flex_shrink 1 ] in
 *
 * Bonsai.Arrow.pipe
 *   (ignore @>> cursor_on |> Bonsai.Arrow.extend_first >>> cutoff >>> component)
 *   ~via:(fun _ (_, _, view) -> [ view ], scroll_props)
 *   ~into:ScrollView.component
 *   ~finalize:(fun _ (value, set_value, _) scroll_view -> value, set_value, scroll_view) *)
