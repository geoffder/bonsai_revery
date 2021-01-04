open Core_kernel
open Import

  type resize =
    [ `Scale of float option * float option
    | `Set of int option * int option
    ]

  type props =
    { styles : Style.t list
    ; attributes : Attributes.t list
    ; max_width : int option
    ; max_height : int option
    }

  val props
    :  ?attributes:Attributes.t list
    -> ?max_width:int
    -> ?max_height:int
    -> Style.t list
    -> props

  val component : (Element.t * props, (resize -> Event.t) * Element.t) Bonsai.t
