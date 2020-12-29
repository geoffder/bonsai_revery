open Core_kernel
open Import

type props =
  { speed : float
  ; styles : Style.t list
  ; attributes : Attributes.t list
  ; min_thumb_size : int
  ; x_slider : Slider.props
  ; y_slider : Slider.props
  }

val props : ?speed:float -> ?attributes:Attributes.t list -> Style.t list -> props

open Core_kernel

val props
  :  ?speed:float
  -> ?attributes:Attributes.t list
  -> ?track_color:Color.t
  -> ?thumb_color:Color.t
  -> ?min_thumb_size:int
  -> ?x_reverse:bool
  -> ?y_reverse:bool
  -> Style.t list
  -> props

val component : (Element.t list * props, Element.t) Bonsai.t
