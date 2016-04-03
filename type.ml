open Support.Pervasive

type ty = TyBool | TyNat | TyArrow of ty * ty | TyAny of string | TyInfer of ty ref

let rec print_type ty = match ty with
    TyBool -> pr "Bool"
  | TyNat -> pr "Nat"
  | TyArrow(ty1,ty2) -> 
      (match ty1 with
          TyArrow _ ->
            pr "("; print_type ty1; pr ")"
        | _ -> print_type ty1);
      pr "->";
      print_type ty2
  | TyAny(name) -> pr "'"; pr name
  | TyInfer(ty) -> print_type !ty

