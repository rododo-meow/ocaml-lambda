open Format
open Support.Error
open Support.Pervasive
open Type

(* ---------------------------------------------------------------------- *)
(* Datatypes *)

type type_term =
    TmBool of info
  | TmNat of info
  | TmArrow of info * type_term * type_term
  | TmNone
  | TmInfered of ty ref

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

type term =
    Exp of exp_term
  | Type of type_term

type command =
  | Eval of info * exp_term

(* ---------------------------------------------------------------------- *)
(* Extracting file info *)

let tmInfo t = match t with
    Exp(TmTrue(fi)) -> fi
  | Exp(TmFalse(fi)) -> fi
  | Exp(TmIf(fi,_,_,_)) -> fi
  | Exp(TmZero(fi)) -> fi
  | Exp(TmSucc(fi,_)) -> fi
  | Exp(TmPred(fi,_)) -> fi
  | Exp(TmIsZero(fi,_)) -> fi 
  | Exp(TmApply(fi,_,_)) -> fi
  | Exp(TmLambda(fi,_,_,_)) -> fi
  | Exp(TmValue(fi,_)) -> fi
  | Type(TmBool(fi)) -> fi
  | Type(TmNat(fi)) -> fi
  | Type(TmArrow(fi,_,_)) -> fi
  | Exp(IllExp) -> dummyinfo

(* ---------------------------------------------------------------------- *)
(* Printing *)

(* The printing functions call these utility functions to insert grouping
  information and line-breaking hints for the pretty-printing library:
     obox   Open a "box" whose contents will be indented by two spaces if
            the whole box cannot fit on the current line
     obox0  Same but indent continuation lines to the same column as the
            beginning of the box rather than 2 more columns to the right
     cbox   Close the current box
     break  Insert a breakpoint indicating where the line maybe broken if
            necessary.
  See the documentation for the Format module in the OCaml library for
  more details. 
*)

let obox0() = open_hvbox 0
let obox() = open_hvbox 2
let cbox() = close_box()
let break() = print_break 0 0

let rec printtm_TypeTerm outer t = match t with
    TmBool(_) -> 
      pr "Bool"
  | TmNat(_) ->
      pr "Nat"
  | TmArrow(_,ty1,ty2) ->
      (match ty1 with
          TmArrow _ ->
            pr "("; printtm_TypeTerm false ty1; pr ")"
        | _ -> printtm_TypeTerm false ty1);
      pr "->";
      printtm_TypeTerm false ty2
  | TmNone -> ()
  | TmInfered(pTy) ->
      print_type !pTy

and printtm_ExpTerm outer t = match t with
    TmApply(fi, t1, t2) ->
      (match t1 with
          TmLambda(_,_,_,_) -> pr "("; printtm_ExpTerm false t1; pr ")"
        | _ -> printtm_ExpTerm false t1);
      pr " ";
      printtm_ExpTerm false t2
  | TmLambda(fi, x, ty, t1) ->
      pr "\\";
      pr x;
      pr ":";
      printtm_TypeTerm false ty;
      pr ".";
      printtm_ExpTerm false t1
  | t -> printtm_Exp outer t

and printtm_Exp outer t = match t with
    TmIf(fi, t1, t2, t3) ->
       pr "if ";
       printtm_ExpTerm false t1;
       pr " then ";
       printtm_ExpTerm false t2;
       pr " else ";
       printtm_ExpTerm false t3;
  | t -> printtm_AppTerm outer t

and printtm_AppTerm outer t = match t with
    TmPred(_,t1) ->
       pr "pred "; printtm_ATerm false t1
  | TmIsZero(_,t1) ->
       pr "iszero "; printtm_ATerm false t1
  | t -> printtm_ATerm outer t

and printtm_ATerm outer t = match t with
    TmTrue(_) -> pr "true"
  | TmFalse(_) -> pr "false"
  | TmZero(fi) ->
       pr "0"
  | TmSucc(_,t1) ->
     let rec f n t = match t with
         TmZero(_) -> pr (string_of_int n)
       | TmSucc(_,s) -> f (n+1) s
       | _ -> (pr "(succ "; printtm_ATerm false t1; pr ")")
     in f 1 t1
  | TmValue(_,v) -> pr v
  | IllExp -> pr "Ill formed exp"
  | t -> pr "("; printtm_ExpTerm outer t; pr ")"

let printtm t = printtm_ExpTerm true t

