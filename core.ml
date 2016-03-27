open Format
open Syntax
open Support.Error
open Support.Pervasive

(* ------------------------   EVALUATION  ------------------------ *)

exception NoRuleApplies
exception ApplyOnNonLambda

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

let rec eval t =
  try let t' = eval1 t
    in eval t'
  with NoRuleApplies -> t
