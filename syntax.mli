(* module Syntax: syntax trees and associated support functions *)

open Support.Pervasive
open Support.Error

(* Data type definitions *)
type term =
    TmTrue of info
  | TmFalse of info
  | TmIf of info * term * term * term
  | TmZero of info
  | TmSucc of info * term
  | TmPred of info * term
  | TmIsZero of info * term
  | TmApply of info * term * term
  | TmLambda of info * string * term
  | TmValue of info * string

type command =
  | Eval of info * term

(* Printing *)
val printtm: term -> unit

(* Misc *)
val tmInfo: term -> info

