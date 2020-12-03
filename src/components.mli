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
    ; styles : Style.t list
    ; attributes : Attributes.t list
    }

  val props : ?speed:float -> ?attributes:Attributes.t list -> Style.t list -> props
  val component : (Element.t Map.M(Int).t * props, Element.t) Bonsai.t
end

module Draggable : sig
  type bounds =
    { x_min : float option
    ; y_min : float option
    ; x_max : float option
    ; y_max : float option
    }

  type props =
    { styles : Style.t list
    ; attributes : Attributes.t list
    ; snap_back : bool
    ; bounds : bounds
    ; on_drag : x:float -> y:float -> Event.t
    ; on_drop : Import.BoundingBox2d.t -> Event.t
    }

  val props
    :  ?attributes:Attributes.t list
    -> ?snap_back:bool
    -> ?bounds:bounds
    -> ?on_drag:(x:float -> y:float -> Event.t)
    -> ?on_drop:(Import.BoundingBox2d.t -> Event.t)
    -> Style.t list
    -> props

  val component : (Element.t * props, BoundingBox2d.t * Element.t) Bonsai.t
end

module Slider : sig
  type props =
    { on_value_changed : float -> Event.t
    ; vertical : bool
    ; min_value : float
    ; max_value : float
    ; init_value : float
    ; slider_length : int
    ; track_thickness : int
    ; track_color : Color.t
    ; thumb : Draggable.props
    }

  val props
    :  ?on_value_changed:(float -> Import.Event.t)
    -> ?vertical:bool
    -> ?min_value:float
    -> ?max_value:float
    -> ?init_value:float
    -> ?slider_length:int
    -> ?thumb_length:int
    -> ?thumb_thickness:int
    -> ?track_thickness:int
    -> ?track_color:Color.t
    -> ?thumb_color:Color.t
    -> unit
    -> props

  val component : (props, float * Element.t) Bonsai.t
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
