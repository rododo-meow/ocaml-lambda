open Support.Pervasive

type ty = TyBool | TyNat | TyArrow of ty * ty | TyAny of string

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

let candidate_name = [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"; "k"; "l"; "m"; "n"; "o" ]

let available = ref candidate_name

let gen_any_type () =
  let name = List.hd !available in
  available := List.tl !available;
  name

let clear_any_type () =
  available := candidate_name
