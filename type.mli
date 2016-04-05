type ty = TyBool | TyNat | TyArrow of ty * ty | TyAny of string

val print_type : ty -> unit

val gen_any_type : ty Map.Make(String).t ref -> string
