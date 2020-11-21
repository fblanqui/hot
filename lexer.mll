{
(** Lexer for HOT files.

HOT, a Higher-Order Termination prover
See the COPYRIGHTS and LICENSE files.

- Frederic Blanqui, 2010-03-15
*)

open Parser;;
open Lexing;;

let incr_line_number lexbuf =
  let ln = lexbuf.lex_curr_p.pos_lnum
  and off = lexbuf.lex_curr_p.pos_cnum in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with
			     pos_lnum = ln+1; pos_bol = off };;

(*REMOVE:let keyword_table = Hashtbl.create 17;;

let _ = List.iter (fun (k, t) -> Hashtbl.add keyword_table k t)
  ["TYPES", TYPES; "FUNS", FUNS; "VARS", VARS; "RULES", RULES];;

let ident s = try Hashtbl.find keyword_table s with Not_found -> IDENT s;;*)

}

let space = [' ' '\r' '\t']
let num = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let letter = alpha | num | '_'| '\'' | '"'
let special = ['!' '#' '$' '%' '&' '+' '<' '?' '@' '\\' '^' '`' '|' '~' '*']
let symb = (special|'-'|'=')+ | (special|'>')+ | '/' 
let ident = letter+ | symb

rule token = parse
  | "//" { one_line_comment lexbuf }
  | "/*" { multi_line_comment lexbuf }
  | '\n' { incr_line_number lexbuf; token lexbuf }
  | space { token lexbuf }
  | '(' { LPAR }
  | ')' { RPAR }
  | '[' { LBRA }
  | ']' { RBRA }
  | ':' { COLON }
  | ';' { SEMICOLON }
  | '.' { DOT }
  | ',' { COMA }
  | "->" { ARROW }
  | "=>" { IMPLY }
  | "TYPES" { TYPES }
  | "FUNS" { FUNS }
  | "VARS" { VARS }
  | "RULES" { RULES }
  | ident as s { IDENT s }
  | eof { EOF }
  | _ as c { Lib.error (Printf.sprintf "'%c' is not a legal character" c) }

and multi_line_comment = parse
  | '\n' { incr_line_number lexbuf; multi_line_comment lexbuf }
  | "*/" { token lexbuf }
  | _ { multi_line_comment lexbuf }

and one_line_comment = parse
  | '\n' { token lexbuf }
  | _ { one_line_comment lexbuf }
