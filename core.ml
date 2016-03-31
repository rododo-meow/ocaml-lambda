open Format
open Syntax
open Support.Error
open Support.Pervasive

(* ------------------------   EVALUATION  ------------------------ *)

exception NoRuleApplies

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

module StringMap = Map.Make(String)

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
  | TmValue(fi,name) -> let i = int_of_string name in TmValue(fi,if i >= lower_bound then string_of_int (i + offset) else name)
  | TmIf(fi,t1,t2,t3) -> TmIf(fi,do_shift t1,do_shift t2,do_shift t3)
  | TmSucc(fi,t1) -> TmSucc(fi,do_shift t1)
  | TmPred(fi,t1) -> TmPred(fi,do_shift t1)
  | TmIsZero(fi,t1) -> TmIsZero(fi,do_shift t1)
  | TmZero(_) -> t
  | TmTrue(_) -> t
  | TmFalse(_) -> t

let rec nameless context t = match t with
    TmLambda(fi,name,ty,t1) -> TmLambda(fi,"",ty,nameless (StringMap.add name 0 (StringMap.map (fun x -> x + 1) context)) t1)
  | TmApply(fi,t1,t2) -> TmApply(fi,nameless context t1,nameless context t2)
  | TmValue(fi,name) -> TmValue(fi,string_of_int(StringMap.find name context))
  | TmIf(fi,t1,t2,t3) -> TmIf(fi,nameless context t1,nameless context t2,nameless context t3)
  | TmSucc(fi,t1) -> TmSucc(fi,nameless context t1)
  | TmPred(fi,t1) -> TmPred(fi,nameless context t1)
  | TmIsZero(fi,t1) -> TmIsZero(fi,nameless context t1)
  | TmZero(_) -> t
  | TmTrue(_) -> t
  | TmFalse(_) -> t

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
  | TmValue(fi,name) -> TmValue(fi,resolve (int_of_string name) context)
  | TmIf(fi,t1,t2,t3) -> TmIf(fi,nameful context t1,nameful context t2,nameful context t3)
  | TmSucc(fi,t1) -> TmSucc(fi,nameful context t1)
  | TmPred(fi,t1) -> TmPred(fi,nameful context t1)
  | TmIsZero(fi,t1) -> TmIsZero(fi,nameful context t1)
  | TmZero(_) -> t
  | TmTrue(_) -> t
  | TmFalse(_) -> t

let rec substitution j s t = match t with
    TmLambda(fi,name,ty,t1) -> TmLambda(fi,name,ty,substitution (j+1) (shift 0 1 s) t1)
  | TmApply(fi,t1,t2) -> TmApply(fi,substitution j s t1, substitution j s t2)
  | TmValue(fi,name) -> if name = string_of_int j then s else t
  | TmIf(fi,t1,t2,t3) -> TmIf(fi,substitution j s t1, substitution j s t2, substitution j s t3)
  | TmSucc(fi,t1) -> TmSucc(fi,substitution j s t1)
  | TmPred(fi,t1) -> TmPred(fi,substitution j s t1)
  | TmIsZero(fi,t1) -> TmIsZero(fi,substitution j s t1)
  | TmZero(_) -> t
  | TmTrue(_) -> t
  | TmFalse(_) -> t

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

let rec print_list l = match l with
    [] -> ()
  | x::xl -> print_string x; print_list xl

let print_map m =
  StringMap.iter (fun s n -> print_string s; print_string ":"; print_int n; print_string "\n") m

let rec nameless_eval t =
  try let t' = eval1 t
    in nameless_eval t'
  with NoRuleApplies -> t

let eval t =
  let context = get_context (get_free_var t)
  in
    nameful context (nameless_eval (nameless context t))
