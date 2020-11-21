module String_of =
struct
  (** Regroups some functions converting to [string]. *)
  open Printf
  open Hs_type

  let rec ty f = function
    | TyArrow (t1, t2) -> sprintf "TyArrow (%s, %s)" (ty f t1) (ty f t2)
    | TyBasic x -> sprintf "TyBasic (%s)" (f x)
    | TyApp (t1, t2) -> sprintf "TyApp (%s, %s)" (ty f t1) (ty f t2)

  let list f l = 
    BatIO.to_string (BatList.print ~first:"[" ~sep:";" ~last:"]" BatIO.nwrite) (BatList.map f l)

  let haskell_decl string_of_t = function 
    | EmptyLine -> "EmptyLine"
    | StartTerm (t1, t2) -> sprintf "StartTerm (%s, %s)" (string_of_t t1) (ty string_of_t t2)
    | ImportQualifiedPrelude -> "ImportQualifiedPrelude"
    | Data (l1, l2) -> sprintf "Data (%s, %s)" (list string_of_t l1) (list (ty string_of_t) l2)
    | TypeAnnot (t1, t2) -> sprintf "TypeAnnot (%s, %s)" (string_of_t t1) (ty string_of_t t2)
    | LhsRhs (t1, t2) -> sprintf "LhsRhs (%s, %s)" (string_of_t t1) (string_of_t t2)

  let t = 
    let rec aux = function
      | Atom a -> sprintf "Atom %S" a
      | List l -> sprintf "List %s" (list aux l) in
    aux

  let haskell = 
    BatIO.to_string (BatOption.print (fun x (Program l) -> BatIO.nwrite x (sprintf "Program %s" (BatIO.to_string (BatList.print (fun n x -> BatIO.nwrite n (haskell_decl t x))) l))))

  let filename ?dir fic = 
    BatIO.to_string (BatEnum.print ~first:"" ~sep:"\n" ~last:"" BatIO.nwrite)
      (BatFile.lines_of (match dir with None -> fic | Some dir -> Printf.sprintf "%s/%s" dir fic))
end


let parse_haskell s = 
  Hs_parser.haskells Hs_lexer.main (Lexing.from_string s)

let parse_haskell_l s = 
  Hs_parser.haskell_line Hs_lexer.main (Lexing.from_string s)
  
open Xtc_of_hs

let parse_haskell_file ?dir fic = 
  let h = parse_haskell (String_of.filename ?dir fic) in
  (*let () = Printf.printf "(* %s *)\n%s\n" fic (String_of.haskell (Some h)) in*)
  try Some (Xtc_of.haskell h) with 
    | Xtc_of.Not_supported err -> 
        let () = Printf.eprintf "warning: not supported (%s) : %s\n" err fic in
        None
    | e -> 
        let () = Printf.eprintf "%s error: %s\n" fic (Printexc.to_string e) in
        None

let main _ = 
  let dir = Sys.argv.(1) in
  BatEnum.fold 
    (fun acc -> function
      | "Makefile" -> acc
      | fic -> parse_haskell_file ~dir fic :: acc)
    []
    (BatSys.files_of dir)
