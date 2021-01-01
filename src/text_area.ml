open Core_kernel
open Import
open Bonsai.Infix
open Components
module Attr = Attributes

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
      | UpdateOffsets
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


  (* FIXME: Would like to wrap long words, rather than extend out of the container and
   * require scrolling, but with how wrapping works in revery, I would need to change a lot
   * of fundamentals here (might in future, to be more like markdown.re to enable better
   * selection highlighting, and other inline formatting etc). If I want it bad enough,
   * consider creating a new wrapping mode in a Revery PR, similar to WrapIgnoreWhiteSpace
   * but with non-hyphenated word breaking when a single word exceeds the available space.
   *
   * NOTE: If edge cases related to overflown lines turn up again, consider trying Wrap mode
   * again, with zero-width unicode whitespace characters instead of spaces in newline_hack.
   * Might work, but haven't tested.
   *
   * TODO: Try making this a bit more efficient by getting all of the indices of spaces, rather
   * than allocating all of the substrings with split. *)
  let measure_text_dims font_info line_height margin text =
    let measure_width = measure_text_width font_info in
    let lines = String.split_lines text in
    let max_x_offset, x_offset, y_count =
      List.fold
        ~init:(0., 0., Int.max 0 (List.length lines - 1))
        ~f:(fun (max_x, x, total_y) line ->
          let inner (acc, overflown, longest, y) word =
            let ext = acc ^ " " ^ word in
            if Float.(measure_width ext > margin)
            then (
              let width = measure_width acc in
              word, Float.(width > margin), Float.max width longest, y + 1 )
            else ext, overflown, longest, y in
          let longest, line_x, line_y =
            match String.split ~on:' ' line with
            | [] -> 0., 0., 0
            | [ h ] ->
              let w = measure_width (if String.equal h "" then " " else h) in
              w, w, 0
            | h :: t ->
              let l, overflown, longest, y' = List.fold ~init:(h, false, 0., 0) ~f:inner t in
              (* Hacky fix to add back in the space which is dropped by inner. Better
               * handling of whitespace (likely drawing from Revery wrapping code) would
               * likely help to avoid this. *)
              (* let w = if overflown then measure_width (" " ^ l) else measure_width l in *)
              let w = measure_width l in
              longest, w, y' in
          Float.max max_x longest, line_x, line_y + total_y)
        lines in
    let y_offset = Float.(of_int y_count * line_height) in
    if (not (String.is_empty text)) && Char.equal '\n' text.[String.length text - 1]
    then max_x_offset, 0., y_offset +. line_height
    else max_x_offset, x_offset, y_offset


  let char_indices char text =
    let len = String.length text in
    let rec loop from idxs =
      if from < len
      then (
        match String.index_from text from char with
        | None -> idxs
        | Some i -> loop (i + 1) (i :: idxs) )
      else idxs in
    match String.index_from text 0 char with
    | None -> []
    | Some i -> List.rev (loop (i + 1) [ i ])


  let measure_text_dims' font_info line_height margin text =
    let measure_width = measure_text_width font_info in
    let lines = String.split_lines text in
    let max_x_offset, x_offset, y_count =
      List.fold
        ~init:(0., 0., Int.max 0 (List.length lines - 1))
        ~f:(fun (max_x, x, total_y) line ->
          let inner (row_start, last_space, overflown, longest, y) next_space =
            let next_width = measure_width (String.slice line row_start next_space) in
            if Float.(next_width > margin)
            then (
              match last_space with
              | Some last ->
                let width = measure_width (String.slice line row_start last) in
                last + 1, None, Float.(width > margin), Float.max width longest, y + 1
              | None -> next_space + 1, None, true, Float.max next_width longest, y + 1 )
            else row_start, Some next_space, overflown, longest, y in
          let longest, line_x, line_y =
            if String.length line > 0
            then (
              match char_indices ' ' line with
              | [] ->
                let w = measure_width line in
                w, w, 0
              | idxs ->
                let row_start, _, overflown, longest, y' =
                  List.fold ~init:(0, None, false, 0., 0) ~f:inner idxs in
                let w = measure_width (Str.string_after line row_start) in
                longest, w, y' )
            else 0., 0., 0 in
          Float.max max_x longest, line_x, line_y + total_y)
        lines in
    let y_offset = Float.(of_int y_count * line_height) in
    if (not (String.is_empty text)) && Char.equal '\n' text.[String.length text - 1]
    then max_x_offset, 0., y_offset +. line_height
    else max_x_offset, x_offset, y_offset


  type row_offsets =
    { start : int
    ; y_offset : float
    ; x_offsets : float list
    }

  let offset_map font_info line_height margin text =
    let measure_width = measure_text_width font_info in
    let f pos (row, row_start, last_space, last_width, offsets, m) c =
      let width = measure_width (String.slice text row_start pos) in
      let offsets = width :: offsets in
      match c with
      | ' ' ->
        if Float.(last_width > margin)
        then
          ( row + 1
          , Option.value ~default:pos last_space + 1
          , None
          , 0.
          , []
          , Map.add_exn
              m
              ~key:row
              ~data:
                { start = row_start
                ; y_offset = Float.of_int row *. line_height
                ; x_offsets = offsets
                } )
        else row, row_start, Some pos, width, offsets, m
      | '\n' ->
        ( row + 1
        , pos + 1
        , None
        , 0.
        , []
        , Map.add_exn
            m
            ~key:row
            ~data:
              { start = row_start; y_offset = Float.of_int row *. line_height; x_offsets = offsets }
        )
      | c -> row, row_start, last_space, width, offsets, m in
    List.foldi ~init:(0, 0, None, 0., [], Map.empty (module Int)) ~f


  let index_nearest_offset' m x_offset y_offset =
    let x_offset = Float.max 0. x_offset in
    let rec find_row last_row i =
      match Map.find m i, last_row with
      | (Some row as current), Some last ->
        if Float.(row.y_offset > y_offset)
        then
          if Float.(row.y_offset - y_offset < y_offset - last.y_offset) then current else last_row
        else find_row current (i + 1)
      | (Some row as current), None -> current
      | _ -> None in
    let%map.Option row = find_row None 0 in
    let rec find_pos i = function
      | [] -> 0
      | [ h ] -> i
      | h0 :: (h1 :: rest as t) ->
        if Float.(h1 > x_offset)
        then if Float.(h1 - x_offset < x_offset - h0) then i + 1 else i
        else find_pos (i + 1) t in
    find_pos 0 (0. :: row.x_offsets) + row.start


  (* NOTE: I'm doing a lot of recalculation vs measure... think about it. *)
  let line_widths font_info margin text =
    let measure_width = measure_text_width font_info in
    let lines = String.split_lines text in
    List.fold
      ~init:[]
      ~f:(fun widths line ->
        let inner (acc, last_width, ws) word =
          let ext = acc ^ " " ^ word in
          let w = measure_width ext in
          if Float.(w > margin) then word, 0., last_width :: ws else ext, w, ws in
        match String.split ~on:' ' line with
        | [] -> widths
        | [ h ] -> measure_width h :: widths
        | h :: t ->
          let l, _, ws = List.fold ~init:(h, 0., widths) ~f:inner t in
          measure_width l :: ws)
      lines
    |> List.rev


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


  let zero_space = "\xe2\x80\x8b"

  (* Add spaces between consecutive newline characters to ensure they are not collapsed/combined by
     Revery text wrapping. *)
  let newline_hack text =
    let f (was_newline, acc) c =
      let is_newline = Char.equal '\n' c in
      ( is_newline
      , if was_newline && is_newline then acc ^ zero_space ^ "\n" else acc ^ String.of_char c )
    in
    let _, hacked = String.fold ~f ~init:(false, "") text in
    if String.equal (Str.last_chars hacked 1) "\n" then hacked ^ zero_space else hacked


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
        Event.Many [ inject Unselect; update value cursor_position ] in

    let handle_text_input (event : Node_events.Text_input.t) =
      let value, cursor_position =
        match model.select_start with
        | Some pos -> remove_between value pos cursor_position
        | None -> value, cursor_position in
      let value, cursor_position = insertString value event.text cursor_position in
      Event.Many [ inject Unselect; update value cursor_position ] in

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
          let new_position = vertical_nav ~up:true model.text_node font_info cursor_position value in
          Event.Many (update value new_position :: selection_event keyboard_event.shift new_position)
        | Down ->
          let new_position =
            vertical_nav ~up:false model.text_node font_info cursor_position value in
          Event.Many (update value new_position :: selection_event keyboard_event.shift new_position)
        | Delete ->
          let value, cursor_position =
            match model.select_start with
            | Some pos -> remove_between value pos cursor_position
            | None ->
              if keyboard_event.ctrl
              then remove_word_after value cursor_position
              else removeCharacterAfter value cursor_position in
          Event.Many [ inject Unselect; update value cursor_position ]
        | Backspace ->
          let value, cursor_position =
            match model.select_start with
            | Some pos -> remove_between value pos cursor_position
            | None ->
              if keyboard_event.ctrl
              then remove_word_before value cursor_position
              else removeCharacterBefore value cursor_position in
          (* NOTE: Important to unselect first, so select_start can't be above end of text.
           * Consider select related flag for update, or altering the actions themselves. *)
          Event.Many [ inject Unselect; update value cursor_position ]
        | V when keyboard_event.ctrl -> paste value cursor_position
        | C when keyboard_event.ctrl ->
          Option.iter model.select_start ~f:(fun pos -> copy_selected value pos cursor_position);
          Event.no_op
        | Return when keyboard_event.shift ->
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
        let measure = measure_text_dims' font_info line_height margin in
        let scene_offsets = (node#getSceneOffsets () : UI.Offset.t) in
        let x_text_offset = mouseX -. Float.of_int scene_offsets.left in
        let y_text_offset = mouseY -. Float.of_int scene_offsets.top +. model.y_scroll in
        let new_position = index_nearest_offset ~measure x_text_offset y_text_offset value in
        let max_x_offset, _, max_y_offset = measure value in
        let text_height = max_y_offset +. line_height in

        let handle_mouse_move
            (pos, x_scroll, y_scroll, last) ({ mouseX; mouseY; _ } : Node_events.Mouse_move.t)
          =
          let now = Time.now () in
          if Float.(Time.Span.to_ms (Time.diff now last) > 35.)
          then (
            (* TODO: In the spirit of making this more efficient, and trying to solve the
             * freezing on large values issue, I should use a specialized nearest offset
             * function which uses precomputed offsets of each character. Two arrays or Maps?
             * Then while moving around, the cursor position simply needs to be looked up.
             *
             * Prob not necessary, but such lookup structures could be stored in the model
             * whenever the value actually changes. *)
            let _, x_offset, y_offset = measure (String.sub ~pos:0 ~len:pos value) in
            let x_scroll = horizontal_scroll margin max_x_offset x_offset x_scroll in
            let y_scroll = vertical_scroll box_height text_height line_height y_offset y_scroll in
            let x_text_offset = mouseX -. Float.of_int scene_offsets.left +. x_scroll in
            let y_text_offset = mouseY -. Float.of_int scene_offsets.top +. y_scroll in
            let new_pos = index_nearest_offset ~measure x_text_offset y_text_offset value in
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
        let margin = Float.of_int (node#measurements ()).width in
        let measure = measure_text_dims font_info line_height margin in
        let _, first_x, first_y = measure (String.sub ~pos:0 ~len:first value) in
        let _, last_x, last_y = measure (String.sub ~pos:0 ~len:last value) in
        let start_line = Int.of_float (first_y /. line_height) in
        (* NOTE: Think about more efficient, clean options. This calculates
         * width for ALL rows. *)
        let widths = line_widths font_info margin value in
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
              ; on_dimensions_changed (fun d -> inject UpdateOffsets)
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
                        ; text_wrap Wrap
                        ; transform [ TranslateX (-.model.x_scroll); TranslateY (-.model.y_scroll) ]
                        ; min_height (Int.of_float (measure_text_height font_info))
                        ]
                  ; kind (TextNode font_info)
                  ]
                (if show_placeholder then props.placeholder else newline_hack value)
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
      schedule_event (inject UpdateOffsets);
      { model with value = Some value; cursor_position }
    | Set_value value ->
      let cursor_position = min model.cursor_position (String.length value) in
      schedule_event (inject UpdateOffsets);
      { model with value = Some value; cursor_position }
    | UpdateOffsets ->
      let value = Option.value ~default:"" model.value in
      let cursor_position = min model.cursor_position (String.length value) in
      ( match Option.bind model.text_node ~f:(fun n -> n#getParent ()) with
      | Some node ->
        let font_info = get_font_info props.attributes in
        let container : UI.Dimensions.t = node#measurements () in
        let margin = Float.(of_int container.width) in
        let line_height = get_line_height font_info node in
        let _, x_offset, y_offset =
          measure_text_dims
            font_info
            line_height
            margin
            (String.sub ~pos:0 ~len:cursor_position value) in
        let max_x_offset, _, max_y_offset = measure_text_dims font_info line_height margin value in
        let x_scroll = horizontal_scroll margin max_x_offset x_offset model.x_scroll in
        let y_scroll =
          vertical_scroll
            (Float.of_int container.height)
            (max_y_offset +. line_height)
            line_height
            y_offset
            model.y_scroll in
        { model with x_offset; y_offset; x_scroll; y_scroll }
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
