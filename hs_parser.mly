%{
type ('a, 'b) sum = 
  | Left of 'a
  | Right of 'b

let case_r f = function
  | Left _ -> assert false
  | Right x -> f x

let case_l f = function
  | Left x -> f x
  | Right _ -> assert false

let map_r f = case_r (fun x -> Right (f x))

let cons_l f l_term = Left (f (List.rev l_term))

open Hs_type
%}

%token <string> STRING
%token LPAREN RPAREN LCOMMENT RCOMMENT COLONCOLON SHARP EQUAL ARROW SEMICOLON OR
%token NEWLINE EOF
%token HTERMINATION DATA IMPORT QUALIFIED

%start terms
%type <Hs_type.t list> terms

%start haskell_line
%type <Hs_type.t Hs_type.haskell_declaration> haskell_line

%start haskells
%type <Hs_type.haskell> haskells
%%

term
  : STRING { Atom $1 }
  | LPAREN RPAREN { List [] }
  | LPAREN rev_terms_aux RPAREN { List (List.rev $2) }

rev_terms_aux
  : term { [$1] }
  | rev_terms_aux term { $2 :: $1 }

terms
  : rev_terms_aux { List.rev $1 }
  | EOF { [] }

ty_data
  : ty { [$1] }
  | ty_data OR ty { $3 :: $1 }

ty
  : ty_simple { $1 }
  | ty_app { $1 }

ty_simple
  : STRING { TyBasic (Atom $1) }
  | LPAREN ty RPAREN { $2 }
  | ty_simple ARROW ty { TyArrow ($1, $3) }

ty_app
  : ty_app ty_simple { TyApp ($1, $2) }
  | ty_simple ty_simple { TyApp ($1, $2) }

hsk
  : term { Right [ $1 ] }
  | hsk COLONCOLON ty { case_r (cons_l (fun l_term -> TypeAnnot (List l_term, $3))) $1 }
  | hsk EQUAL terms { case_r (cons_l (fun l_term -> LhsRhs (List l_term, List $3))) $1 }
  | hsk term { map_r (fun l_term -> $2 :: l_term) $1 }

haskell_line
  : LCOMMENT SHARP HTERMINATION LPAREN terms COLONCOLON ty RPAREN SHARP RCOMMENT { StartTerm (List $5, $7) }
  | IMPORT QUALIFIED STRING { ImportQualifiedPrelude }
  | DATA terms EQUAL ty_data { Data ($2, List.rev $4) }
  | hsk { case_l (fun x -> x) $1 }
  | haskell_line SEMICOLON { $1 }

newlines
  : NEWLINE { () }
  | newlines NEWLINE { () }

haskell_aux
  : haskell_line { [$1] }
  | haskell_aux newlines haskell_line { $3 :: $1 }

haskells
  : haskell_aux { Program (List.rev $1) }
  | haskell_aux newlines { Program (List.rev $1) }
  | EOF { Program [] }
