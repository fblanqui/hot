(**
HOT, a Higher-Order Termination prover
See the COPYRIGHTS and LICENSE files.

- Frederic Blanqui, 2011-08-09
*)

open Util;;
open Term;;
open Expr;;
open Prec;;
open Lib;;

(*****************************************************************************)
(** the set FO of first-order function symbols if the biggest subset
    of function symbols having a first-order type and in the defining
    rules of which there is no abstraction, no applied variables and only
    function symbols of FO *)
(*****************************************************************************)

let get_fo_funs, set_fo_funs = get_set StrSet.empty;;

(** a quasi first-order type is a type of the form [t0 => .. => tn]
    with every ti being base types. it is a first-order type if all the
    base types occurring in it are first-order. a base type is
    first-order if it is basic, i.e. has no higher-order
    constructor. *)
let is_quasi_fo_typ (ts, _) = List.for_all is_base_typ ts;;

(** a term is quasi first-order if it contains no abtraction and no
    applied variable and all its function symbols have a first-order
    type *)
let rec is_quasi_fo_term = function
  | Symb (Fun f), ts ->
      let tf = typ_of_ident f in
	is_quasi_fo_typ tf && arity_typ tf = List.length ts
	  && List.for_all is_quasi_fo_term ts
  | Symb (Var _), [] -> true
  | (Symb (Var _)|Abs _), _ -> false;;

(** a rule is quasi first-order if both its left and right-hand sides
    are quasi first-order terms *)
let is_quasi_fo_rule (l, r) = is_quasi_fo_term l && is_quasi_fo_term r;;

(** a function symbol is quasi first-order if it has a first-order
    type and all its defining rules are quasi first-order *)
let is_quasi_fo_ident f =
  is_quasi_fo_typ (typ_of_ident f)
  && List.for_all is_quasi_fo_rule (rules_of_ident f);;

(** quasi first-order undefined symbols are first-order *)
let compute_fo_undef_funs() =
  StrSet.filter (fun f -> is_quasi_fo_typ (typ_of_ident f)) (get_undef_funs());;

(** given a set of symbols [s] try to find in [get_def_prec()] a
    symbol [f] that is not in [s] but whose predecessors are all in
    [s] *)
let fo_funs_find s =
  try
    StrMap.iter
      (fun f m ->
	 if not (StrSet.mem f s) && is_quasi_fo_ident f
	   && StrMap.for_all (fun g c -> c <> Gt || StrSet.mem g s) m
	 then found f)
      (get_def_prec());
    None
  with Found f -> (*debugf "other first-order symbol: " string f;*) Some f;;

let rec fo_funs_iter s =
  match fo_funs_find s with
    | Some f -> fo_funs_iter (StrSet.add f s)
    | None -> s;;

(** first-order symbols *)
let compute_fo_funs() =
  verbose "compute first-order symbols...";
  set_fo_funs (fo_funs_iter (compute_fo_undef_funs()));
  verbosef "first-order symbols: " (set ",") (get_fo_funs());;

(*****************************************************************************)
(** convert a FO problem into a HOT problem *)
(*****************************************************************************)

type fo_problem = int StrMap.t * rules;;

let id_o = "o";;
let typ_o = typ_const id_o;;

let rec typ_arity k = if k > 0 then arrow typ_o (typ_arity (k-1)) else typ_o;;

let pb_of_fo_pb (fo_sig, rs) =
  { sig_typs = StrSet.singleton id_o;
    sig_funs = StrMap.fold
      (fun f k m -> StrMap.add f (typ_arity k) m) fo_sig StrMap.empty;
    sig_vars = StrSet.fold
      (fun x m -> StrMap.add x typ_o m) (vars_rules rs) StrMap.empty },
  rs;;

(*****************************************************************************)
(** call the external first-order prover *)
(*****************************************************************************)

let is_set_fo_prover, get_fo_prover, set_fo_prover =
  is_set_get_set "first-order prover" "";;

let get_fop_result fn =
  let result =
    try
      let ic = open_in fn in
      let s = input_line ic in
	close_in ic;
	s
    with _ -> "MAYBE"
  in
    verbose ("result: " ^ result);
    match result with
      | "YES" -> Yes
      | "NO" -> No
      | _ -> Maybe;;

let call_fo_prover fop p =
  (* the file extension ".xtc" does not work with TTT2 *)
  let xtcfn = new_temp "hot" ".xml" in
    output_file_xtc xtcfn (Xtc_of_hot.fo_problem p);
    if get_debug() then output_file_hot
      (new_temp_change_postfix xtcfn ".hot") (pb_of_fo_pb p);
    let outfn = new_temp_change_postfix xtcfn ".out" in
    let cmd = Printf.sprintf "%s %s > %s 2>&1" fop xtcfn outfn in
    let _ = command cmd in
      get_fop_result outfn;;

(*****************************************************************************)
(** try to prove the termination of the FO part *)
(*****************************************************************************)

(** signatures and rules of FO symbols *)
let fo_pb() =
  StrSet.fold
    (fun id (m, rs) -> StrMap.add id (arity_fun id) m, rules_of_ident id @ rs)
    (get_fo_funs())
    (StrMap.empty, []);;

(** extend a signature with a new binary symbol and the corresponding
    projection rules *)
let ce_ext =
  let x = Symb (Var "x"), [] and y = Symb (Var "y"), [] in
  let xy = [x; y] in
    fun (fo_sig, rs) ->
      let f = fresh "p" in
      let l = Symb (Fun f), xy in
	StrMap.add f 2 fo_sig, (l,x) :: (l,y) :: rs;; 

(** compute the set of all defined symbols *)
let compute_all_def_funs() =
  StrMap.fold (fun id _ s -> StrSet.add id s) (get_rules_map()) StrSet.empty;;

(** compute the set of all rules *)
let compute_all_rules() =
  StrMap.fold (fun _ rs rules -> rs @ rules) (get_rules_map())
    (get_other_rules());;

(** compute the set of all non first-order defined symbols *)
let compute_ho_def_funs() =
  let fo_funs = get_fo_funs() in
    StrMap.fold
      (fun id _ s -> if StrSet.mem id fo_funs then s else StrSet.add id s)
      (get_rules_map()) StrSet.empty;;

(** say if a symbol has no duplicating rules *)
let is_non_dup_fun id = List.for_all is_non_dup (rules_of_ident id);;

let try_prove_fo_part() =
  let (_fo_sig, fo_rules) as x = fo_pb() in
    if fo_rules = [] then begin
      verbose "there is no first-order rule";
      set_funs (compute_all_def_funs());
    end else if is_set_fo_prover() then begin
      (* we first test for ce-termination *)
      verbose "try to prove ce-termination of first-order rules...";
      match call_fo_prover (get_fo_prover()) (ce_ext x) with
	| Yes -> (* we can remove all first-order rules *)
	    verbose "first-order rules ce-terminate";
	    verbose "remove first-order rules";
	    set_funs (compute_ho_def_funs());
	    add_comp_funs (get_fo_funs())
	| No|Maybe -> (* we then test for termination *)
	    verbose "try to prove termination of first-order rules...";
	    match call_fo_prover (get_fo_prover()) x with
	      | Yes ->
		  verbose "first-order rules terminate";
		  if is_orthogonal_alg (compute_all_rules()) then begin
		    verbose "the system is orthogonal";
		    (* we can remove all FO rules *)
		    verbose "remove first-order rules";
		    set_funs (compute_ho_def_funs());
		    add_comp_funs (get_fo_funs())
		  end else begin
		    (* we can remove all the rules defining FO symbols
		       having no duplicating rules *)
		    verbose "remove non-duplicating first-order rules";
		    let ns, ds =
		      StrSet.partition is_non_dup_fun (get_fo_funs())
		    in set_funs (StrSet.union (compute_ho_def_funs()) ds);
		      add_comp_funs ns
		  end
	      | No -> verbose "first-order rules do not terminate"; quit No
	      | Maybe -> ()
    end else begin
      verbose ("no external first-order prover has been specified");
      set_funs (compute_all_def_funs())
    end;;
