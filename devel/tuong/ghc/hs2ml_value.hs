module Hs2ml_value where
import Language.Haskell.Exts
import Language.Haskell.Exts.Syntax
import Control.Monad
{-
-- purely form
_name n = case n of
  Ident s -> s
_literal l = case l of
  Int i -> show i
  String s -> show s
_qname q = case q of
  UnQual n -> _name n
_pat p = case p of
  PVar n -> _name n
_list beg sep end f l = 
  beg ++ (case l of 
             [] -> "" 
             x:xs -> foldl (\acc x -> acc ++ sep ++ f x) (f x) xs) ++ end
_fieldupdate f = case f of
  FieldUpdate q e -> _qname q ++ " = " ++ _exp e
_exp e = case e of
  App e1 e2 -> 
    let app_left e nb acc = case e of
          App e1 e2 -> app_left e1 (succ nb) (e2 : acc)
          _ -> (e, nb, acc) in
    let zero = 0 in
    let (e, nb, l) = app_left e1 zero [e2] in
    if nb == zero then
      _exp e ++ " " ++ _exp e2
    else
      _exp e ++ " " ++ _list "(" ", " ")" _exp l
  Con q -> _qname q
  Lit l -> _literal l
  List l -> _list "[ " " ; " " ]" _exp l
  Paren e -> p_exp e
  RecConstr q l -> "(*" ++ _qname q ++ "*)" ++ _list "{ " " ; " " }" _fieldupdate l
p_exp e = "(" ++ _exp e ++ ")"

main (ParseOk e) = putStr (_exp e)
{-
real    14m27.615s
user    12m34.080s
sys     4m55.280s
-}
-}


-- monadic form
_name (Ident s) = putStr s
_literal l = putStr $ case l of
  Int i -> show i
  String s -> show s
_qname (UnQual n) = _name n
_pat (PVar n) = _name n
_list beg sep end f l = do
  putStr beg 
  case l of 
    [] -> putStr "" 
    x:xs -> do
      f x
      foldM (\_ x -> do
                putStr sep
                f x) () xs
  putStr end
_fieldupdate (FieldUpdate q e) = do 
  _qname q 
  putStr " = "
  _exp e
_exp e = case e of
  App e1 e2 -> 
    let app_left e acc = case e of
          App e1 e2 -> app_left e1 (e2 : acc)
          _ -> (e, acc) in
    let (e, l) = app_left e1 [e2] in do
    _exp e
    putStr " "
    case l of
      [_] -> _exp e2
      _ -> _list "(" ", " ")" _exp l
  Con q -> _qname q
  Lit l -> _literal l
  List l -> _list "[ " " ; " " ]" _exp l
  Paren e -> do
    putStr "("
    _exp e 
    putStr ")"
  RecConstr q l -> do 
    putStr "(*" 
    _qname q 
    putStr "*)"
    _list "{ " " ; " " }" _fieldupdate l

main (ParseOk e) = _exp e
{-
real    9m4.565s
user    7m28.240s
sys     3m39.700s
-}
