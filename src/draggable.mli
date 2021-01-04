open Core_kernel
open Import

type freedom =
  | X
  | Y
  | Free

type props =
  { styles : Style.t list
  ; attributes : Attributes.t list
  ; freedom : freedom
  ; snap_back : bool
  ; on_drag : bb:BoundingBox2d.t -> x:float -> y:float -> Event.t
  ; on_drop : bb:BoundingBox2d.t -> x:float -> y:float -> Event.t
  }

val props
  :  ?attributes:Attributes.t list
  -> ?freedom:freedom
  -> ?snap_back:bool
  -> ?on_drag:(bb:BoundingBox2d.t -> x:float -> y:float -> Event.t)
  -> ?on_drop:(bb:BoundingBox2d.t -> x:float -> y:float -> Event.t)
  -> Style.t list
  -> props

val component
  : ( Element.t * props
    , BoundingBox2d.t option
      * (BoundingBox2d.t -> Event.t)
      * (float -> float -> Event.t)
      * Element.t )
    Bonsai.t
