open Import

val box : Attributes.t list -> Element.t list -> Element.t
val text : Attributes.t list -> string -> Element.t
val image : Attributes.t list -> Element.t
val opacity : ?opacity:float -> Element.t list -> Element.t
val tick : Element.t -> every:Core_kernel.Time.Span.t -> Element.t

val button
  :  ?disabled:bool
  -> ?disabled_attr:Attributes.t list
  -> (hovered:bool -> Attributes.t list)
  -> string
  -> Element.t

val clickable_box
  :  ?key:Brisk_reconciler.Key.t
  -> ?disabled:bool
  -> Attributes.attributes
  -> Element.t
  -> Element.t

val mouse_capture
  :  ?on_mouse_down:('a -> Node_events.Mouse_button.t -> Event.t * 'a option)
  -> ?on_mouse_up:('a -> Node_events.Mouse_button.t -> Event.t * 'a option)
  -> ?on_mouse_move:('a -> Node_events.Mouse_move.t -> Event.t * 'a option)
  -> ?on_mouse_wheel:('a -> Node_events.Mouse_wheel.t -> Event.t * 'a option)
  -> ?on_release:('a option -> Event.t)
  -> 'a
  -> unit

module Expert : sig
  type 'a component =
    ?key:UI.React.Key.t
    -> (('a, 'a) UI.React.Hooks.t -> Element.t * (UI.React.Hooks.nil, 'a) UI.React.Hooks.t)
    -> Element.t

  val make_component : use_dynamic_key:bool -> 'a component

  val box
    :  ?key:int
    -> 'a component
    -> Attributes.t list
    -> (('a, 'a) UI.React.Hooks.t -> Element.t list * (UI.React.Hooks.nil, 'a) UI.React.Hooks.t)
    -> Element.t
end
