module Str = 
struct 
  include Str

  let l_match = List.for_all (fun (r, x) -> Str.string_match r x 0)

  let str_match r s l = 
    if Str.string_match (Str.regexp r) s 0 then
      Some (List.map (fun i -> Str.matched_group i s) l)
    else 
      None
end

open Printf
open Hs_type

type ('a, 'b) opt = 
  | S of 'a
  | N of 'b

type 'a haskell_declaration =
  | Old_EmptyLine
  | Old_StartTerm of 'a * 'a
  | Old_ImportQualifiedPrelude
  | Old_Data of 'a * 'a
  | Old_TypeAnnot of 'a * 'a
  | Old_LhsRhs of 'a * 'a
  | Old_UnknownRaw of string

let parse_hs parse_string file =

  let str_match r s = 
    Str.str_match r s (List.tl (BatList.mapi (fun i _ -> i) (BatString.nsplit r "\\(.+\\)"))) in

  BatEnum.map (function
      | "" -> Old_EmptyLine
      | s -> 
    match str_match "{-# htermination (\\(.+\\)::\\(.+\\)) #-}" s with
      | Some [s1; s2] -> Old_StartTerm (parse_string s1, parse_string s2)
      | _ -> 
    let s = BatString.strip ~chars:";" s in
    let str r = str_match r s in
    match str "import qualified Prelude" with
      | Some [] -> Old_ImportQualifiedPrelude
      | _ -> 
    match str "data \\(.+\\)=\\(.+\\)" with
      | Some [s1; s2] -> Old_Data (parse_string s1, parse_string s2)
      | _ -> 
    match str "\\(.+\\)::\\(.+\\)" with
      | Some [s1; s2] -> Old_TypeAnnot (parse_string s1, parse_string s2)
      | _ -> 
    match str "\\(.+\\)=\\(.+\\)" with
      | Some [s1; s2] -> Old_LhsRhs (parse_string s1, parse_string s2)
      | _ -> Old_UnknownRaw s
  ) (BatFile.lines_of file)

open Xtc

type ('a, 'b) haskell_decl = 
  | Fundef of 'a
  | Typedef of 'b

let fundef_of_lhsrhs lhs rhs = 
  let rec aux_hs = function
    | Atom s :: xs -> TermFunapp (s, List.map (function Atom s -> TermVar s | List l -> aux_hs l) xs)
    | [] -> assert false in
  Fundef (aux_hs lhs, aux_hs rhs)

let typedef_of_typeannot [Atom s] t1 = 
  let rec aux_ty l = 
    let split_arrow l = 
      let add l all_l = List.rev l :: all_l in
      let l, all_l = 
        List.fold_left (fun (l, all_l) -> function
          | Atom "->" -> [], add l all_l
          | x -> x :: l, all_l
        ) ([], []) l in
      List.rev (add l all_l) in
    match split_arrow l with
      | [l] -> 
        (* type apply *)
        TypeBasic "2do"
      | l -> (* if l = [] then assert false *)
        match
          BatList.fold_right (fun a ->
              let a0 = aux_ty a in
              function 
                | None -> Some a0
                | Some a1 -> Some (TypeArrow (a0, a1))) l None
        with
          | None -> assert false
          | Some t -> t in
  Typedef (s, aux_ty t1)


let f_import acc = function 
  | Old_LhsRhs (List lhs, List rhs) -> fundef_of_lhsrhs lhs rhs :: acc
  | Old_TypeAnnot (List t1, List t2) -> typedef_of_typeannot t1 t2 :: acc
  | _ -> acc

    
(* *)

let parse_string s = 
  let scan_terms lexbuf = Hs_parser.terms Hs_lexer.main lexbuf in
  try Some (List (scan_terms (Lexing.from_string s)))
  with _ -> None

let import_fic acc dir s = 
  let p =
    parse_hs 
       (fun s -> 
         match parse_string s with
           | Some s -> s
           | None -> assert false)
       (sprintf "%s/%s" dir s) in  
  BatEnum.fold f_import acc p

let main_dir () = 
  let dir = Sys.argv.(1) in
  let l = 
    BatEnum.fold
      (fun acc -> function
        | "Makefile" -> acc
        | s -> import_fic acc dir s
      ) []
      (BatSys.files_of dir) in
  ignore l

let _ = 
  if Sys.is_directory Sys.argv.(1) then
    ignore (Conv_hs.main ())
  else
    ignore (Conv_hs.parse_haskell_file Sys.argv.(1))

(*
let _ = 
  let l = main_fic () in
  let () = ignore l in
  ()
*)
