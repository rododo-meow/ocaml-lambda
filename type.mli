type ty = TyBool | TyNat | TyArrow of ty * ty | TyAny of string

val print_type : ty -> unit

val gen_any_type : unit -> string

val clear_any_type : unit -> unit
