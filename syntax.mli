(* module Syntax: syntax trees and associated support functions *)

open Support.Pervasive
open Support.Error

(* Data type definitions *)
type type_term =
    TmBool of info
  | TmNat of info
  | TmArrow of info * type_term * type_term

type exp_term =
    TmTrue of info
  | TmFalse of info
  | TmIf of info * exp_term * exp_term * exp_term
  | TmZero of info
  | TmSucc of info * exp_term
  | TmPred of info * exp_term
  | TmIsZero of info * exp_term
  | TmApply of info * exp_term * exp_term
  | TmLambda of info * string * type_term * exp_term
  | TmValue of info * string
  | IllExp

type term = Exp of exp_term | Type of type_term

type command =
  | Eval of info * exp_term

(* Printing *)
val printtm: exp_term -> unit

(* Misc *)
val tmInfo: term -> info

