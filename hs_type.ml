type t = 
  | Atom of string
  | List of t list

type 'a ty = 
  | TyArrow of 'a ty * 'a ty
  | TyBasic of 'a
  | TyApp of 'a ty * 'a ty

type 'a haskell_declaration = 
  | EmptyLine
  | StartTerm of 'a * 'a ty
  | ImportQualifiedPrelude
  | Data of 'a list * 'a ty list 
  | TypeAnnot of 'a * 'a ty
  | LhsRhs of 'a * 'a

type haskell = 
  | Program of t haskell_declaration list
