open Printf

module type GHCI = 
sig
  val ghci : string list -> string list * Unix.process_status 
    (** [ghci l] calls GHCI, passing l as argument. 
        [l] and the answer are in natural top-down order.  *)
end

module Ghci : GHCI = 
struct
  let ghci l = 
    let ic, oc = Unix.open_process "ghci -v0 -O2 -fobject-code" in
    let () =
      begin
        List.iter (fun s -> 
          begin 
            output_string oc s;
            output_char oc '\n';
          end) l;
        close_out oc;
      end in
    let l = 
      let rec aux l = 
        match try Some (input_line ic) with _ -> None with
          | None -> l
          | Some s -> aux (s :: l) in
      aux [] in
    List.rev l, Unix.close_process (ic, oc)
end

let ghci l = 
  match 
    Ghci.ghci ("import Language.Haskell.Exts"
               :: "import Language.Haskell.Exts.Syntax"
               :: l)
  with
    | l, Unix.WEXITED 0 -> l
    | _ -> failwith "unexpected exit status of 'ghci'"

let filename fic = Filename.chop_extension (Filename.basename fic)

let ml_module s = 
  let s = filename s in
  String.make 1 (Char.uppercase s.[0]) ^ String.sub s 1 (pred (String.length s))



let hs_dir = (** directory to import *)
  let hs_dir = "/home/f/tmp/build/tpdb-8.0/FP/basic_haskell" in
  if Sys.file_exists hs_dir then
    hs_dir
  else
    let _ = printf "Where are the '.hs' files ? [default = %s] %!" hs_dir in
    read_line ()
let name_type = "type.ml"
let name_value = "value.ml"
let hs2ml_value = "hs2ml_value"
let hs2ml_type = "hs2ml_type"
let ast_parsed_hs = "ast_parsed_hs.hs.data"

let import_value oc = 
(*
  let ghci _ = (* failing form *)
  [ Printf.sprintf "ParseFailed ({srcFilename = %S; srcLine = %d; srcColumn = %d}, %S)" "" min_int min_int "" ] in
*)
  let l_fic_hs = (* contains a list of HS files *)
    let t = Sys.readdir hs_dir in
    let _ = Array.fast_sort (fun a b -> compare (String.lowercase b) (String.lowercase a)) t in
    Array.fold_left (fun acc -> function "Makefile" -> acc | s -> s :: acc) [] t in

  let lg = List.length l_fic_hs in

  let _ = fprintf oc "open %s\n" (ml_module name_type) in
  ignore (
    List.fold_left (fun nb fic -> 
      let _ = Printf.printf "%d (%d) %s %!" nb lg (filename fic) in

      let _ = 
        begin
          output_string oc (sprintf "let _%s = " (filename fic));
          List.iter (fun s -> 
            begin
              output_string oc s;
              output_char oc '\n';
            end)
            (ghci [ sprintf ":load %s" hs2ml_value
                  ; sprintf "do f <- parseFile \"%s/%s\" ; main (parseExp (show f))" hs_dir fic ]);
        end in
(*
      let p_float = Printf.printf "%.07f %!" in

      let tm = Unix.gettimeofday () in
      let _ = 
        begin
          ignore (ghci [ sprintf "do f <- parseFile \"%s/%s\" ; return $ parseExp (show f)" hs_dir fic ]);
        end in
      let _ = p_float (Unix.gettimeofday () -. tm) in

      let tm = Unix.gettimeofday () in
      let _ = 
        begin
          ignore (ghci [ sprintf "parseFile \"%s/%s\"" hs_dir fic ]);
        end in
      let _ = p_float (Unix.gettimeofday () -. tm) in

      let tm = Unix.gettimeofday () in
      let _ = 
        begin
          ignore (Xtc_of_hs.import_hs hs_dir fic);
        end in
      let _ = p_float (Unix.gettimeofday () -. tm) in

      let _ = Printf.printf "%d\n%!" (Unix.stat (sprintf "%s/%s" hs_dir fic)).Unix.st_size in
*)
      succ nb
    ) 1 l_fic_hs)

let import_type oc = 
  List.iter (fprintf oc "%s\n")
    (ghci [ sprintf ":load %s" hs2ml_type
          ; sprintf "do f <- parseFile \"%s\" ; main f" ast_parsed_hs ])

let _ =

  let oc = open_out name_value in
  let () = import_value oc in
  let () = close_out oc in

  let oc = open_out name_type in
  let () = import_type oc in
  let () = close_out oc in

  ()
