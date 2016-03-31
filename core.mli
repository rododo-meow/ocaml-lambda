(* module Core

   Core typechecking and evaluation functions
*)

open Syntax
open Support.Error

val eval : exp_term -> exp_term 
