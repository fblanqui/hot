%{
(** Parser for HOT files.

HOT, a Higher-Order Termination prover
See the COPYRIGHTS and LICENSE files.

- Frederic Blanqui, 2010-03-15
*)

open Term;;
open Expr;;
open Lib;;
open Util;;

%}

%token EOF COLON SEMICOLON COMA DOT ARROW IMPLY
%token LPAR RPAR LBRA RBRA
%token TYPES FUNS VARS RULES
%token <string> IDENT

%type <Expr.expr_problem> pb

%start pb /* entry point */

%%

pb:
| typ_part fun_part var_part rule_part
    { { sig_typs = $1; sig_funs = $2; sig_vars = $3 }, $4 }
;
typ_part:
| /* nothing */ { StrSet.empty }
| TYPES typ_decls SEMICOLON { $2 }
;
typ_decls:
| IDENT { StrSet.singleton $1 }
| IDENT COMA typ_decls { set_add_chk $1 $3 }
;
fun_part:
| /* nothing */ { StrMap.empty }
| FUNS fun_decls { $2 }
;
fun_decls:
| /* nothing */ { StrMap.empty }
| fun_decl fun_decls { merge_chk $1 $2 }
;
fun_decl:
| idents COLON typ SEMICOLON
    { List.fold_left (fun m id -> add_chk id $3 m) StrMap.empty $1 }
;
idents:
| IDENT { [$1] }
| IDENT COMA idents { $1 :: $3 }
;
typ:
| IDENT { typ_const $1 }
| LPAR typ RPAR { $2 }
| typ IMPLY typ { arrow $1 $3 }
;
var_part:
| /* nothing */ { StrMap.empty }
| VARS var_decls { $2 }
;
var_decls:
| /* nothing */ { StrMap.empty }
| var_decl var_decls { merge_chk $1 $2 }
;
var_decl:
| idents COLON typ SEMICOLON
    { List.fold_left (fun m id -> add_chk id $3 m) StrMap.empty $1 }
;
rule_part:
| /* nothing */ { [] }
| RULES rules { $2 }
;
rules:
| /* nothing */ { [] }
| rule rules { $1 :: $2 }
;
rule:
| lhs ARROW rhs SEMICOLON { $1, $3 }
;
lhs:
| term_no_par { $1 }
;
rhs:
| term_no_par { $1 }
;
term_no_par:
| head terms { expr_app $1 $2 }
;
head:
| IDENT { EIdent $1 }
| LPAR abs_no_par RPAR { $2 }
;
abs_no_par:
| IDENT COLON typ DOT term_no_par { EAbs ($1, $3, $5) }
;
terms:
| /*nothing*/ { [] }
| term terms { $1 :: $2 }
;
term:
| head { $1 }
| LPAR term_no_par RPAR { $2 }
;
