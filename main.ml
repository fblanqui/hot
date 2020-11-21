(** Main procedure.

HOT, a Higher-Order Termination prover
See the COPYRIGHTS and LICENSE files.

- Frederic Blanqui, 2010-03-15

    Read command line options, call parsing functions and the main
    procedure for trying to prove termination.
*)

open Printf;;
open Arg;;
open Lexing;;
open Expr;;
open Util;;
open Lib;;

(*****************************************************************************)
(** functions for setting parameters *)
(*****************************************************************************)

let set_debug_mode() =
  (*ignore (Parsing.set_trace true);*) set_debug(); set_verbose();;

let is_set_input_file, get_input_file, set_input_file =
  is_set_get_set "input file name" "";;

type input_type = InXtc | InHot | InHaskell;;

let is_set_input_type, get_input_type, set_input_type =
  is_set_get_set "input file type" InXtc;;

type action = OutHot | OutXtc | Termin;;

let get_action, set_action = get_set Termin;;

let curry = ref true;;

let set_with_arity() = curry := false;;
let set_curry() = curry := true;;

(*****************************************************************************)
(** command line parsing *)
(*****************************************************************************)

let usage_msg = "usage: " ^ Sys.argv.(0) ^ " [options] [filename]\n";;

let rec options() =
  List.sort (fun (x,_,_) (y,_,_) -> Pervasives.compare x y) (Arg.align
[
  "-h", Unit print_help,
  " display the list of options";
  "-d", Unit set_debug_mode,
  " debugging mode";
  "-xtc", Unit (fun () -> set_input_type InXtc),
  " take as input a file in the XTC format";
  "-hot", Unit (fun () -> set_input_type InHot),
  " take as input a file in the HOT format";
  "-hs", Unit (fun () -> set_input_type InHaskell),
  " take as input a file in the Haskell format";
  "-ohot", Unit (fun () -> set_action OutHot),
  " convert the input file into the HOT format";
  "-oxtc", Unit (fun () -> set_action OutXtc),
  " convert the input file into the XTC format";
  "-v", Unit set_verbose,
  " verbose mode";
  "-fop", String Fo.set_fo_prover,
  " use <string> as external first-order prover";
  "-t", Unit set_test_mode,
  " output some tests and exit";
  "-lps", String set_lp_solver,
  " use <string> as external linear program solver";
  "-curry", Unit set_curry,
  " output XTC terms in Curry form (default behavior)";
  "-with-arity", Unit set_with_arity,
  " output XTC terms with maximal function applications";
])

and print_options oc =
  fprintf oc "options:\n";
  List.iter (fun (k, s, d) ->
               match s with
                 | String _ -> fprintf oc "%s <string>: %s\n" k d
                 | Unit _ | Bool _ | Set _ | Clear _ | Set_string _ | Int _
                 | Set_int _ | Float _ | Set_float _ | Tuple _ | Symbol _
                 | Rest _ -> fprintf oc "%s: %s\n" k d)
    (options())

and print_help() = print_endline usage_msg; print_options stdout; exit 0;;

let options = options();;

let parse_args() =
  verbose "parse command line arguments...";
  Arg.parse options set_input_file usage_msg;;

(*****************************************************************************)
(** HOT file parsing *)
(*****************************************************************************)

let lex_pos b l =
  bprintf b "file \"%s\", line %d, character %d: syntax error"
    l.pos_fname l.pos_lnum (l.pos_cnum - l.pos_bol + 1);;

let parse_hot_lexbuf lb =
  try Parser.pb Lexer.token lb
  with _ -> errorf "" lex_pos lb.lex_curr_p;;

let parse_hot ic =
  let lb = Lexing.from_channel ic in
    lb.lex_curr_p <- { lb.lex_curr_p with pos_fname = get_input_file() };
    parse_hot_lexbuf lb;;

(*****************************************************************************)
(** XTC file parsing *)
(*****************************************************************************)

let parse_xml ic =
  try Xtc_of_xml.problem (Libxml.parse_xml ic)
  with Error.Error x as e -> Error.print_error x; raise e;;

let parse_xtc ic = Hot_of_xtc.expr_problem_of_xtc_problem (parse_xml ic);;

(*****************************************************************************)
(** Haskell file parsing *)
(*****************************************************************************)

let parse_hs ic =
  Hot_of_xtc.expr_problem_of_xtc_problem
    (Xtc_of_hs.Xtc_of.haskell (Xtc_of_hs.parse_haskell_ic ic))

(*****************************************************************************)
(** parsing functions *)
(*****************************************************************************)

let parse_file parse_channel fn =
  let ic = open_in fn in
  let x = parse_channel ic in
    close_in ic; x;;

let parse_type = function
  | InXtc -> parse_xtc
  | InHot -> parse_hot
  | InHaskell -> parse_hs;;

let parse_pb() =
  verbose "parse input file...";
  parse_file (parse_type (get_input_type())) (get_input_file());;

(*****************************************************************************)
(** main procedure *)
(*****************************************************************************)

let out_hot epb = verbose "output HOT..."; fprint expr_problem stdout epb;;

let xtc_of_hot p =
  if !curry then
    (verbose "function symbols are taken of arity 0 (option -curry)";
     Xtc_of_hot.cproblem p)
  else
    (verbose "function symbols are taken of maximal arity (option -with-arity)";
     Xtc_of_hot.aproblem p);;

let out_xtc (sg, rs) =
  build_symb_typ_map sg;
  compute_rules rs;
  verbose "output XTC...";
  Libxml.output_xml stdout (Xml_of_xtc.problem (xtc_of_hot (sg, get_rules())));;

let termin (sg, rs) =
  build_symb_typ_map sg;
  compute_rules rs;
  (*verbose "check that rules preserve typing...";
  if not (preserve_typ_rules (get_rules())) then
    (verbose "rules do not preserve typing"; quit Maybe);*)
  build_maps();
  if get_input_type() = InHot then notin (keys (get_symb_typ_map()));
    (* for InXtc, it is already done in hot_of_xtc *)
  Sn.try_prove_termin();
  Loop.try_prove_non_termin();
  verbose "give up";
  quit Maybe;;

(*KEEP? let test_hot (sg, rs) =
  build_symb_typ_map sg;
  compute_rules rs;
  let x = Xml_of_xtc.problem (xtc_of_hot (sg, get_rules())) in
  let sg', rs' = Hot_of_xtc.expr_problem_of_xtc_problem x in
    if not (sg = sg' && (rs = rs' || rs = List.rev rs')) then
      (print_endline "test failed"; exit 1);;*)

let do_action = function
  | OutHot -> out_hot
  | OutXtc -> out_xtc
  | Termin -> termin;;

let main() =
  parse_args();
  if get_test_mode() then (print_tests(); exit 0);
  if not(is_set_input_type()) then error "missing input type";
  if not(is_set_input_file()) then error "missing input file";
  do_action (get_action()) (parse_pb());;

(*****************************************************************************)
(* launch the main procedure *)
(*****************************************************************************)

let do_at_exit() = print_buffer(); if not (get_debug()) then remove_temps();;

let _ = at_exit do_at_exit;;

let _ = main();;
