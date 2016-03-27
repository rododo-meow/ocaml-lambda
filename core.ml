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

let up context lower_bound offset =
  StringMap.map (fun x -> if x >= lower_bound then x + offset else x) context

let rec nameless context t = match t with
    TmLambda(fi,name,t1) -> TmLambda(fi,"",nameless (StringMap.add name 0 (up context 0 1)) t1)
  | TmApply(fi,t1,t2) -> TmApply(fi,nameless context t1,nameless context t2)
  | TmValue(fi,name) -> TmValue(fi,string_of_int(StringMap.find name context))

let apply t1 t2 = match t1 with
    TmLambda(fi,name,t1) -> t1
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
    nameless (get_context (get_free_var t)) t
