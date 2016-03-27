/*  
 *  Yacc grammar for the parser.  The files parser.mli and parser.ml
 *  are generated automatically from parser.mly.
 */

%{
open Support.Error
open Support.Pervasive
open Syntax
%}

/* ---------------------------------------------------------------------- */
/* Preliminaries */

/* We first list all the tokens mentioned in the parsing rules
   below.  The names of the tokens are common to the parser and the
   generated lexical analyzer.  Each token is annotated with the type
   of data that it carries; normally, this is just file information
   (which is used by the parser to annotate the abstract syntax trees
   that it constructs), but sometimes -- in the case of identifiers and
   constant values -- more information is provided.
 */

/* Keyword tokens */

/* Identifier and constant value tokens */
%token <string Support.Error.withinfo> ID
%token <int Support.Error.withinfo> INTV
%token <float Support.Error.withinfo> FLOATV
%token <string Support.Error.withinfo> STRINGV

/* Symbolic tokens */
%token <Support.Error.info> DOT
%token <Support.Error.info> EOF
%token <Support.Error.info> LAMBDA
%token <Support.Error.info> SEMI
%token <Support.Error.info> LPAREN
%token <Support.Error.info> RPAREN

/* ---------------------------------------------------------------------- */
/* The starting production of the generated parser is the syntactic class
   toplevel.  The type that is returned when a toplevel is recognized is
   Syntax.command list.
*/

%start toplevel
%type < Syntax.command list > toplevel
%%

/* ---------------------------------------------------------------------- */
/* Main body of the parser definition */

/* The top level of a file is a sequence of commands, each terminated
   by a semicolon. */
toplevel :
    EOF
      { [] }
  | Command SEMI toplevel
      { let cmd = $1 in
          let cmds = $3 in
          cmd::cmds }

/* A top-level command */
Command :
  | Term 
      { (let t = $1 in Eval(tmInfo t,t)) }

Term :
    LPAREN Term RPAREN  
      { $2 } 
  | Term Term
      { TmApply(tmInfo $2, $1, $2) }
  | LAMBDA ID DOT Term
      { TmLambda($1, $2.v, $4) }
  | ID
      { TmValue($1.i, $1.v) }

/*   */
