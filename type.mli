type ty = TyBool | TyNat | TyArrow of ty * ty | TyAny of string | TyInfer of ty ref

val print_type : ty -> unit
