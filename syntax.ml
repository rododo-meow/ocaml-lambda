open Format
open Support.Error
open Support.Pervasive

(* ---------------------------------------------------------------------- *)
(* Datatypes *)

type term =
  | TmApply of info * term * term
  | TmLambda of info * string * term
  | TmValue of info * string

type command =
  | Eval of info * term

(* ---------------------------------------------------------------------- *)
(* Extracting file info *)

let tmInfo t = match t with
    TmApply(fi,_,_) -> fi
  | TmLambda(fi,_,_) -> fi
  | TmValue(fi,_) -> fi

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

let rec printtm_Term outer t = match t with
    TmApply(fi, t1, t2) ->
      (match t1 with
          TmLambda(_,_,_) -> pr "("; printtm_Term false t1; pr ")"
        | TmApply(_,_,_) -> printtm_Term false t1
        | TmValue(_,_) -> printtm_Term false t1);
      pr " ";
      printtm_Term false t2
  | TmLambda(fi, x, t1) ->
      pr "\\";
      pr x;
      pr ".";
      printtm_Term false t1
  | TmValue(fi, name) ->
      pr name

let printtm t =
  printtm_Term true t;

