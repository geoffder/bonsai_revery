open Core_kernel
open Import

type props =
  { autofocus : bool
  ; cursor_color : Color.t
  ; placeholder : string
  ; placeholder_color : Color.t
  ; default_value : string option
  ; on_key_down : Node_events.Keyboard.t -> string -> (string -> Event.t) -> Event.t
  ; attributes : Attributes.t list
  }

val props
  :  ?autofocus:bool
  -> ?cursor_color:Color.t
  -> ?placeholder:string
  -> ?placeholder_color:Color.t
  -> ?default_value:string
  -> ?on_key_down:(Node_events.Keyboard.t -> string -> (string -> Event.t) -> Event.t)
  -> Attributes.t list
  -> props

val component : (props, string * (string -> Event.t) * Element.t) Bonsai.t
