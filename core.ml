open Format
open Syntax
open Support.Error
open Support.Pervasive

(* ------------------------   EVALUATION  ------------------------ *)

exception NoRuleApplies
exception ApplyOnNonLambda

let rec get_free_var t = match t with
    TmLambda(_,name,t1) -> List.filter (fun x -> (String.compare x name) != 0) (get_free_var t1)
  | TmApply(_,t1,t2) -> List.sort_uniq Pervasives.compare (List.concat [ (get_free_var t1); (get_free_var t2) ])
  | TmValue(_,name) -> [ name ]

module StringMap = Map.Make(String)

let get_context free_vars =
  let rec get_context' free_vars n map = match free_vars with
      [] -> map
    | var::free_vars' -> get_context' free_vars' (n+1) (StringMap.add var n map)
  in
    get_context' free_vars 0 StringMap.empty

let rec shift lower_bound offset t = match t with
    TmApply(fi,t1,t2) -> TmApply(fi,shift lower_bound offset t1,shift lower_bound offset t2)
  | TmLambda(fi,name,t1) -> TmLambda(fi,name,shift (lower_bound+1) offset t1)
  | TmValue(fi,name) -> let i = int_of_string name in TmValue(fi,if i >= lower_bound then string_of_int (i + offset) else name)

let rec nameless context t = match t with
    TmLambda(fi,name,t1) -> TmLambda(fi,"",nameless (StringMap.add name 0 (StringMap.map (fun x -> x + 1) context)) t1)
  | TmApply(fi,t1,t2) -> TmApply(fi,nameless context t1,nameless context t2)
  | TmValue(fi,name) -> TmValue(fi,string_of_int(StringMap.find name context))

let rec substitution j s t = match t with
    TmLambda(fi,name,t1) -> TmLambda(fi,name,substitution (j+1) (shift 0 1 s) t1)
  | TmApply(fi,t1,t2) -> TmApply(fi,substitution j s t1, substitution j s t2)
  | TmValue(fi,name) -> if name = string_of_int j then s else t

let apply t1 t2 = match t1 with
    TmLambda(fi,name,t1) -> shift 0 (-1) (substitution 0 (shift 0 1 t2) t1)
  | _ -> raise ApplyOnNonLambda

let rec eval1 t = match t with
    TmApply(fi,t1,t2) ->
      (try
        TmApply(fi,(eval1 t1),t2)
      with NoRuleApplies -> apply t1 t2)
  | TmLambda(_,_,_) -> 
      raise NoRuleApplies
  | TmValue(_,_) ->
      raise NoRuleApplies

let rec print_list l = match l with
    [] -> ()
  | x::xl -> print_string x; print_list xl

let print_map m =
  StringMap.iter (fun s n -> print_string s; print_string ":"; print_int n; print_string "\n") m

let rec eval t =
  let context = get_context (get_free_var t)
  in
    print_map context;
    eval1 (nameless (get_context (get_free_var t)) t)
