open Core_kernel
open Import

type length =
  | Dynamic of int
  | Static of int

type props =
  { on_value_changed : float -> Event.t
  ; vertical : bool
  ; reverse : bool
  ; min_value : float
  ; max_value : float
  ; init_value : float
  ; slider_length : length
  ; track_thickness : int
  ; track_color : Color.t
  ; thumb : Draggable.props
  }

val props
  :  ?on_value_changed:(float -> Import.Event.t)
  -> ?vertical:bool
  -> ?reverse:bool
  -> ?min_value:float
  -> ?max_value:float
  -> ?init_value:float
  -> ?slider_length:length
  -> ?thumb_length:length
  -> ?thumb_thickness:int
  -> ?track_thickness:int
  -> ?track_color:Color.t
  -> ?thumb_color:Color.t
  -> unit
  -> props

val component : (props, float * Element.t) Bonsai.t

val with_thumb
  : ( BoundingBox2d.t option
      * (BoundingBox2d.t -> Event.t)
      * (float -> float -> Event.t)
      * Element.t
      * props
    , float * Element.t )
    Bonsai.t
