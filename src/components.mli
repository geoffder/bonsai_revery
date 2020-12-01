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

module Text_input : sig
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
end

module ScrollView : sig
  open Core_kernel

  type props =
    { speed : float
    ; style : Style.t list
    }

  module T : sig
    module Action : sig
      type t
    end
  end

  val inject_child_dims
    :  (T.Action.t -> Bonsai_revery__Import.Event.t)
    -> int
    -> Revery_UI__NodeEvents.DimensionsChangedEventParams.t
    -> Bonsai_revery__Import.Event.t

  val props : ?speed:float -> Style.t list -> props

  val component
    : ( ((T.Action.t -> Bonsai_revery__Import.Event.t) -> Element.t Map.M(Int).t) * props
      , Element.t )
      Bonsai.t
end

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
