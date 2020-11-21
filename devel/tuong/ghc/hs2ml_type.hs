module Hs2ml_type where
import Language.Haskell.Exts
import Language.Haskell.Exts.Syntax
import Data.Char

_list beg sep end f l = 
  beg ++ (case l of 
             [] -> "" 
             x:xs -> foldl (\acc x -> acc ++ sep ++ f x) (f x) xs) ++ end

mot_list = "List"

_type t = case t of
  TyVar (Ident s) -> "'" ++ s
  TyCon (UnQual (Ident s)) -> _ident s
  TyList t -> _type (TyApp (TyCon (UnQual (Ident mot_list))) t)
  TyParen t -> "(" ++ _type t ++ ")"
  TyApp t1 (t2 @ (TyTuple _ l)) -> _list "(" " * " ")" _type l ++ _type t1
  TyApp t1 t2 -> "(" ++ _type t2 ++ ")" ++ _type t1
  TyTuple _ l -> _list "(" ", (* a *)" ")" _type l
  
_ident (n:ns) = '_' : n : ns

mot_cle = ""

_decl d = case d of
  DataDecl _  DataType [] (Ident n) l_param l_body _ ->
    mot_cle ++ (case l_param of 
                   [] -> "" 
                   _ -> _list "(" ", " ")" (\ (UnkindedVar (Ident s)) -> "'" ++ s) l_param ++ " ") ++ _ident n ++ " = " ++
    _list "" "" "" (\ e -> case e of
                       QualConDecl _ [] [] (ConDecl (Ident "GenPragma") [UnBangedTy (TyCon (UnQual (Ident "String"))),UnBangedTy (TyTuple Boxed [TyCon (UnQual (Ident "Int")),TyCon (UnQual (Ident "Int"))]),UnBangedTy (TyTuple Boxed [TyCon (UnQual (Ident "Int")),TyCon (UnQual (Ident "Int"))]),UnBangedTy (TyCon (UnQual (Ident "Exp")))]) -> "(* 2do *)"
                       QualConDecl _ [] [] (ConDecl (Ident cons) lapp ) -> 
                         "\n  | " ++ cons ++ (case lapp of 
                                               [] -> "" 
                                               _ -> " of " ++ _list "" " * " "" (\ (UnBangedTy ty) -> _type ty) lapp)
                       QualConDecl _ [] [] (RecDecl _ l) -> 
                         _list "{" " ; " "}" (\ ([Ident n],UnBangedTy ty) -> n ++ " : " ++ _type ty) l
                   ) l_body
  DataDecl _ NewType [] (Ident "ModuleName") [] [QualConDecl _ [] [] (ConDecl (Ident "ModuleName") [UnBangedTy (TyCon (UnQual (Ident "String")))])] [] -> "_ModuleName = ModuleName of _String"
  TypeDecl _ (Ident n) _ (TyTuple Boxed lapp) -> 
    mot_cle ++ _ident n ++ " = " ++ _list "" " * " "" _type lapp
  TypeDecl _ (Ident n) _ ty -> 
    mot_cle ++ _ident n ++ " = " ++ _type ty
    
_decl_list = 
  _list (_list "" "\n" "" id [ "type _String = string"
                             , "type 'a " ++ _ident mot_list ++ " = 'a list"
                             , "type 'a _Maybe = Nothing | Just of 'a"
                             , "type _Bool = True | False"
                             , "type _Int = int"
                             , "type _Integer"
                             , "type _Char"
                             , "type _Rational"
                             , ""
                             , "(* generated part *)"
                             , "type "]) "\nand " "" _decl

main (ParseOk (Module _ (ModuleName "Main") [] Nothing (Just [EVar (UnQual (Ident "main"))]) [] l)) = 
  putStrLn $ _decl_list l
