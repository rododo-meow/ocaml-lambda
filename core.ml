open Format
open Syntax
open Support.Error
open Support.Pervasive
open Type

(* ------------------------   EVALUATION  ------------------------ *)

exception NoRuleApplies
exception IllFormed
exception MatchFail

module StringMap = Map.Make(String)
module Int = struct
  type t = int
  let compare i j = Pervasives.compare i j
end
module IntMap = Map.Make(Int)

let is_any ty = match ty with
    TyAny _ -> true
  | _ -> false

let get_name ty = match ty with
    TyAny(name) -> name
  | _ -> pr "Not an Any type"; force_newline(); raise (Exit 1)

let get_pos t = match t with
    TmValue(_,name) -> int_of_string (String.sub name 1 ((String.length name)-1))
  | _ -> pr "Not a value"; force_newline(); raise (Exit 1)

let rec traversal pinfer ty = match ty with
    TyArrow(ty1,ty2) -> TyArrow(traversal pinfer ty1, traversal pinfer ty2)
  | TyAny(name) ->
    let resolved = StringMap.find name !pinfer in
    if ((is_any resolved) && ((get_name ty) = (get_name resolved))) then ty else traversal pinfer resolved
  | _ -> ty

let rec type_term_to_type pinfer t =
  match t with
    TmBool(_) -> TyBool
  | TmNat(_) -> TyNat
  | TmArrow(_,ty1,ty2) -> TyArrow(type_term_to_type pinfer ty1, type_term_to_type pinfer ty2)
  | TmNone(name) ->
    traversal pinfer (StringMap.find name !pinfer)

let rec type_to_type_term t =
  match t with
    TyBool -> TmBool(dummyinfo)
  | TyNat -> TmNat(dummyinfo)
  | TyArrow(ty1,ty2) -> TmArrow(dummyinfo,type_to_type_term ty1, type_to_type_term ty2)
  | TyAny(name) -> TmNone(name)

let infer pinfer name ty =
   pinfer := StringMap.add name ty !pinfer

let rec match_infer pinfer ty1 ty2 = match ty1 with
    TyNat -> (match ty2 with
        TyNat -> ()
      | TyAny(name) -> infer pinfer name TyNat
      | _ -> raise MatchFail)
  | TyBool -> (match ty2 with
        TyBool -> ()
      | TyAny(name) -> infer pinfer name TyBool
      | _ -> raise MatchFail)
  | TyArrow(ty11,ty12) -> (match ty2 with
        TyArrow(ty21,ty22) -> match_infer pinfer ty11 ty21; match_infer pinfer ty12 ty22
      | TyAny(name) -> infer pinfer name ty1
      | _ -> raise MatchFail)
  | TyAny(name1) -> (match ty2 with
        TyAny(name2) -> if name1 = name2 then () else if name1 < name2 then infer pinfer name1 ty2 else infer pinfer name2 ty1
      | _ -> match_infer pinfer ty2 ty1)

let rec get_type context pinfer t = match t with
    TmIf(_,t1,t2,t3) ->
      (match get_type context pinfer t1 with
          TyBool ->
            let ty1 = get_type context pinfer t2
            and ty2 = get_type context pinfer t3
            in 
            if ty1 = ty2 then ty1 else
              (printInfo (tmInfo (Exp t));
               pr "Type of two branches of if doesn't match"; force_newline();
               pr "True:  "; print_type ty1; force_newline();
               pr "False: "; print_type ty2; force_newline();
               raise IllFormed)
        | TyAny(name) ->
           (pinfer := StringMap.add name TyBool !pinfer;
            get_type context pinfer t)
        | ty ->
          (printInfo (tmInfo (Exp t));
           pr "The condition exp's type is "; print_type ty; force_newline();
           raise IllFormed))
  | TmZero(_) -> TyNat
  | TmSucc(_,t) -> 
    (match get_type context pinfer t with
       TyNat -> TyNat
     | TyAny(name) ->
       (match_infer pinfer (TyAny(name)) TyNat;
        TyNat)
     | ty ->
       (printInfo (tmInfo (Exp t));
        pr "Apply succ on "; print_type ty; force_newline();
        raise IllFormed))
  | TmPred(_,t) ->
    (match get_type context pinfer t with
       TyNat -> TyNat
     | TyAny(name) ->
       (match_infer pinfer (TyAny(name)) TyNat;
        TyNat)
     | ty ->
       (printInfo (tmInfo (Exp t));
        pr "Apply pred on "; print_type ty; force_newline();
        raise IllFormed))
  | TmIsZero(_,t') ->
    (match get_type context pinfer t' with
       TyNat -> TyBool
     | TyAny(name) ->
       (match_infer pinfer (TyAny(name)) TyNat;
        TyBool)
     | ty ->
       (printInfo (tmInfo (Exp t));
        pr "Apply iszero on "; print_type ty; force_newline();
        raise IllFormed))
  | TmTrue(_) -> TyBool
  | TmFalse(_) -> TyBool
  | TmValue(_,name) -> 
    (try 
       let ty = traversal pinfer (IntMap.find (get_pos t) context) in
       if (is_any ty) then type_term_to_type pinfer (TmNone(get_name ty)) else ty
     with
       Not_found ->
       (printInfo (tmInfo (Exp t));
        pr "Free variable"; force_newline();
        raise IllFormed))
  | TmApply(_,t1,t2) ->
    (let ty1 = get_type context pinfer t1
     and ty2 = get_type context pinfer t2
     in
       (if is_any ty1 then
          (match_infer pinfer ty1 (TyArrow(ty2,TyAny(gen_any_type pinfer)));
           get_type context pinfer t)
        else match ty1 with
            TyArrow(_tys,_tyd) ->
            let tys = traversal pinfer _tys
            and tyd = traversal pinfer _tyd
            in if tys = ty2 then tyd
            else if (is_any tys) then
              (match_infer pinfer tys ty2;
               get_type context pinfer t)
            else
              (try
                 match_infer pinfer tys ty2;
                 get_type context pinfer t
               with 
                 MatchFail ->
                 (printInfo (tmInfo (Exp t));
                  pr "Abstraction's type doesn't match with actual argument (need "; print_type tys; pr ", got "; print_type ty2; pr ")"; force_newline();
                  raise IllFormed))
          | _ ->
            (printInfo (tmInfo (Exp t));
             pr "Do apply on "; print_type ty1; force_newline();
             raise IllFormed)))
  | TmLambda(_,name,ty,t2) ->
    (let ty1 = type_term_to_type pinfer ty
     in let ty2 = get_type (IntMap.add 0 ty1 (IntMap.fold (fun key ty m -> IntMap.add (key+1) ty m) context IntMap.empty)) pinfer t2
     in TyArrow(ty1, ty2))
  | IllExp -> raise IllFormed

let rec get_free_var t = match t with
    TmLambda(_,name,_,t1) -> List.filter (fun x -> (String.compare x name) != 0) (get_free_var t1)
  | TmApply(_,t1,t2) -> List.sort_uniq Pervasives.compare (List.concat [ (get_free_var t1); (get_free_var t2) ])
  | TmValue(_,name) -> [ name ]
  | TmIf(_,t1,t2,t3) -> List.sort_uniq Pervasives.compare (List.concat [ (get_free_var t1); (get_free_var t2); (get_free_var t3) ])
  | TmSucc(_,t1) -> get_free_var t1
  | TmPred(_,t1) -> get_free_var t1
  | TmIsZero(_,t1) -> get_free_var t1
  | TmZero(_) -> []
  | TmTrue(_) -> []
  | TmFalse(_) -> []
  | IllExp -> []

let get_context free_vars =
  let rec get_context' free_vars n map = match free_vars with
      [] -> map
    | var::free_vars' -> get_context' free_vars' (n+1) (StringMap.add var n map)
  in
    get_context' free_vars 0 StringMap.empty

let rec shift lower_bound offset t = 
  let do_shift t =
    shift lower_bound offset t
  in match t with
    TmApply(fi,t1,t2) -> TmApply(fi,do_shift t1,do_shift t2)
  | TmLambda(fi,name,ty,t1) -> TmLambda(fi,name,ty,shift (lower_bound+1) offset t1)
  | TmValue(fi,name) -> let i = get_pos t in TmValue(fi,if i >= lower_bound then String.concat "" [ "&" ; string_of_int (i + offset)] else name )
  | TmIf(fi,t1,t2,t3) -> TmIf(fi,do_shift t1,do_shift t2,do_shift t3)
  | TmSucc(fi,t1) -> TmSucc(fi,do_shift t1)
  | TmPred(fi,t1) -> TmPred(fi,do_shift t1)
  | TmIsZero(fi,t1) -> TmIsZero(fi,do_shift t1)
  | TmZero(_) -> t
  | TmTrue(_) -> t
  | TmFalse(_) -> t
  | IllExp -> t

let rec nameless context t = match t with
    TmLambda(fi,name,ty,t1) -> TmLambda(fi,"",ty,nameless (StringMap.add name 0 (StringMap.map (fun x -> x + 1) context)) t1)
  | TmApply(fi,t1,t2) -> TmApply(fi,nameless context t1,nameless context t2)
  | TmValue(fi,name) -> TmValue(fi,String.concat "" [ "&" ; string_of_int(StringMap.find name context)])
  | TmIf(fi,t1,t2,t3) -> TmIf(fi,nameless context t1,nameless context t2,nameless context t3)
  | TmSucc(fi,t1) -> TmSucc(fi,nameless context t1)
  | TmPred(fi,t1) -> TmPred(fi,nameless context t1)
  | TmIsZero(fi,t1) -> TmIsZero(fi,nameless context t1)
  | TmZero(_) -> t
  | TmTrue(_) -> t
  | TmFalse(_) -> t
  | IllExp -> t

let resolve i context =
  match List.hd (List.filter (fun (key,value) -> value = i) (StringMap.bindings context)) with
  (key,value) -> key

let candidate_name = [ "a"; "b"; "c"; "d"; "e"; "f" ]

let generate_name context =
  let used_names = List.map (fun (key,value) -> key) (StringMap.bindings context)
  in
    List.hd (List.filter (fun name -> not (List.exists (fun name2 -> name = name2) used_names)) candidate_name)

let rec nameful context t = match t with
    TmLambda(fi,name,ty,t1) ->
      let generated_name = generate_name context
      in TmLambda(fi,generated_name,ty,nameful (StringMap.add generated_name 0 (StringMap.map (fun x -> x+1) context)) t1)
  | TmApply(fi,t1,t2) -> TmApply(fi,nameful context t1,nameful context t2)
  | TmValue(fi,name) -> TmValue(fi,resolve (get_pos t) context)
  | TmIf(fi,t1,t2,t3) -> TmIf(fi,nameful context t1,nameful context t2,nameful context t3)
  | TmSucc(fi,t1) -> TmSucc(fi,nameful context t1)
  | TmPred(fi,t1) -> TmPred(fi,nameful context t1)
  | TmIsZero(fi,t1) -> TmIsZero(fi,nameful context t1)
  | TmZero(_) -> t
  | TmTrue(_) -> t
  | TmFalse(_) -> t
  | IllExp -> t

let rec substitution j s t = match t with
    TmLambda(fi,name,ty,t1) -> TmLambda(fi,name,ty,substitution (j+1) (shift 0 1 s) t1)
  | TmApply(fi,t1,t2) -> TmApply(fi,substitution j s t1, substitution j s t2)
  | TmValue(fi,name) -> if get_pos t = j then s else t
  | TmIf(fi,t1,t2,t3) -> TmIf(fi,substitution j s t1, substitution j s t2, substitution j s t3)
  | TmSucc(fi,t1) -> TmSucc(fi,substitution j s t1)
  | TmPred(fi,t1) -> TmPred(fi,substitution j s t1)
  | TmIsZero(fi,t1) -> TmIsZero(fi,substitution j s t1)
  | TmZero(_) -> t
  | TmTrue(_) -> t
  | TmFalse(_) -> t
  | IllExp -> t

let apply t1 t2 = match t1 with
    TmLambda(fi,name,_,t1) -> shift 0 (-1) (substitution 0 (shift 0 1 t2) t1)
  | _ -> raise NoRuleApplies

let rec isnumericval t = match t with
    TmZero(_) -> true
  | TmSucc(_,t1) -> isnumericval t1
  | _ -> false

let rec isval t = match t with
    TmTrue(_)  -> true
  | TmFalse(_) -> true
  | t when isnumericval t  -> true
  | _ -> false

let rec eval1 t = match t with
    TmApply(fi,t1,t2) ->
      (try
        TmApply(fi,(eval1 t1),t2)
      with NoRuleApplies -> apply t1 t2)
  | TmLambda(_,_,_,_) -> 
      raise NoRuleApplies
  | TmValue(_,_) ->
      raise NoRuleApplies
  | TmIf(_,TmTrue(_),t2,t3) ->
      t2
  | TmIf(_,TmFalse(_),t2,t3) ->
      t3
  | TmIf(fi,t1,t2,t3) ->
      let t1' = eval1 t1 in
      TmIf(fi, t1', t2, t3)
  | TmSucc(fi,t1) ->
      let t1' = eval1 t1 in
      TmSucc(fi, t1')
  | TmPred(_,TmZero(_)) ->
      TmZero(dummyinfo)
  | TmPred(_,TmSucc(_,nv1)) when (isnumericval nv1) ->
      nv1
  | TmPred(fi,t1) ->
      let t1' = eval1 t1 in
      TmPred(fi, t1')
  | TmIsZero(_,TmZero(_)) ->
      TmTrue(dummyinfo)
  | TmIsZero(_,TmSucc(_,nv1)) when (isnumericval nv1) ->
      TmFalse(dummyinfo)
  | TmIsZero(fi,t1) ->
      let t1' = eval1 t1 in
      TmIsZero(fi, t1')
  | TmZero(_) ->
      raise NoRuleApplies
  | TmTrue(_) ->
      raise NoRuleApplies
  | TmFalse(_) ->
      raise NoRuleApplies
  | IllExp ->
      raise NoRuleApplies

let rec print_list l = match l with
    [] -> ()
  | x::xl -> print_string x; print_list xl

let print_map m =
  StringMap.iter (fun s n -> print_string s; print_string ":"; print_int n; force_newline()) m

let rec nameless_eval t =
  (try let t' = eval1 t
     in
     (pr "= "; printtm t'; force_newline();
      nameless_eval t')
   with NoRuleApplies -> t)

let rec gen_all_any pinfer t = match t with
    TmIf(fi,t1,t2,t3) -> TmIf(fi,gen_all_any pinfer t1,gen_all_any pinfer t2,gen_all_any pinfer t3)
  | TmPred(fi,t) -> TmPred(fi,gen_all_any pinfer t)
  | TmSucc(fi,t) -> TmSucc(fi,gen_all_any pinfer t)
  | TmTrue _ -> t
  | TmFalse _ -> t
  | TmZero _ -> t
  | TmIsZero(fi,t) -> TmIsZero(fi,gen_all_any pinfer t)
  | TmApply(fi,t1,t2) -> TmApply(fi,gen_all_any pinfer t1,gen_all_any pinfer t2)
  | TmLambda(fi,name,ty,body) ->
    (match ty with
       TmNone _ ->
       TmLambda(fi,name,TmNone(gen_any_type pinfer),gen_all_any pinfer body)
     | _ -> t)
  | TmValue _ -> t
  | IllExp -> t

let rec fill_infered_type pinfer t = match t with
    TmIf(fi,t1,t2,t3) -> TmIf(fi,fill_infered_type pinfer t1,fill_infered_type pinfer t2,fill_infered_type pinfer t3)
  | TmPred(fi,t) -> TmPred(fi,fill_infered_type pinfer t)
  | TmSucc(fi,t) -> TmSucc(fi,fill_infered_type pinfer t)
  | TmTrue _ -> t
  | TmFalse _ -> t
  | TmZero _ -> t
  | TmIsZero(fi,t) -> TmIsZero(fi,fill_infered_type pinfer t)
  | TmApply(fi,t1,t2) -> TmApply(fi,fill_infered_type pinfer t1,fill_infered_type pinfer t2)
  | TmLambda(fi,name,ty,body) ->
    (match ty with
       TmNone(name) ->
       TmLambda(fi,"",type_to_type_term (traversal pinfer (StringMap.find name !pinfer)),fill_infered_type pinfer body)
     | _ -> t)
  | TmValue _ -> t
  | IllExp -> t

let eval t =
  let context = get_context (get_free_var t)
  in let type_context = ref StringMap.empty
  in let nameless_rep = nameless context (gen_all_any type_context t)
  and drop v = ()
  in
    try   
      pr "After nameless: "; printtm nameless_rep; force_newline();
      drop (get_type IntMap.empty type_context nameless_rep);
      pr "After type infering: "; printtm (fill_infered_type type_context nameless_rep); force_newline();
      nameful context (nameless_eval nameless_rep)
    with
      IllFormed -> IllExp
