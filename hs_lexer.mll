{
open Hs_parser
}

let newline = '\010' | '\013' | "\013\010"
let space = [' ' '\009' '\012']
let whitespace = [' ' '\010' '\013' '\009' '\012']

rule main = parse
  | newline { NEWLINE }
  | space+ { main lexbuf }

  | '(' { LPAREN }
  | ')' { RPAREN }
  | "{-" { LCOMMENT }
  | "-}" { RCOMMENT }
  | "::" { COLONCOLON }
  | '#' { SHARP }
  | '=' { EQUAL }
  | ';' { SEMICOLON }
  | "->" { ARROW }
  | "|" { OR }

  | "htermination" { HTERMINATION }
  | "data" { DATA }
  | "import" { IMPORT }
  | "qualified" { QUALIFIED }

  | ([^ '(' ')' '{' '#' '}' ':' ';' '=' '-' '>' '|'] # whitespace)+ as str { STRING str }
  | eof { EOF }
