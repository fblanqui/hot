(**
HOT, a Higher-Order Termination prover
See the COPYRIGHTS and LICENSE files.

- Frederic Blanqui, 2011-10-17
*)

open Util;;
open Lib;;
open Term;;
open Expr;;

(*****************************************************************************)
(** rewriting *)
(*****************************************************************************)

let rewrite_hd tm c ((h, _) as r) =
  let rs =
    match h with
      | Symb (Fun f) -> rules_of_ident f
      | Symb (Var _) | Abs _ -> get_other_rules()
  in rewrite_hd typ_term rs tm c r;;

let rewrite_or_beta_hd = union rewrite_hd beta_hd;;

let trace_rewrite_hd tm c (((h, _), _) as r) =
  let rs =
    match h with
      | Symb (Fun f) -> rules_of_ident f
      | Symb (Var _) | Abs _ -> get_other_rules()
  in trace_rewrite_hd typ_term rs tm c r;;

let trace_rewrite_or_beta_hd = union trace_rewrite_hd trace_beta_hd;;

(*****************************************************************************)
(** try to find a loop by exploring reducts of rule lhs *)
(*****************************************************************************)

let max_red = 3;; (** maximum number of reduction steps *)

let exists_subterm_matching t0 =
  exists_subterm bound_vars_typing
    (fun tm t -> matching typ_term tm t0 t <> None);;

let exists_loop_from t0 =
  debugf "\ntry find loop from: " term t0;
  try
    let _ =
      iter max_red
	(function
	   | [] -> []
	   | ls ->
	       (*debugf "\nreducts:\n" (list "\n" term) ls;*)
	       if List.exists (exists_subterm_matching t0) ls
	       then raise Exit
	       else fold_app (reducts rewrite_or_beta_hd) ls)
	(reducts rewrite_or_beta_hd t0)
    in false
  with Exit -> true;;

let trace_exists_loop_from t0 =
  debugf "from: " term t0;
  try
    let _ =
      iter max_red
	(function
	   | [] -> []
	   | l ->
	       (*debugf "\nreducts:\n" (list "\n" (first term)) l;*)
	       try verbosef "found loop:\n" loop
		 (List.find (fun (t, _) -> exists_subterm_matching t0 t) l);
		 raise Exit
	       with Not_found ->
		 fold_app (trace_reducts trace_rewrite_or_beta_hd) l)
	(trace_reducts trace_rewrite_or_beta_hd (t0, []))
    in false
  with Exit -> true;;

let gen_subs k l =
  let vs = StrSet.elements (vars l) in
  let make_sub =
    List.fold_left2 (fun s v t -> StrMap.add v t s) StrMap.empty vs in
    List.map (fun ts -> subs (make_sub ts) l)
      (enum (List.map (fun v -> Db.gen_terms k (typ_of_ident v)) vs));;

let check_subs k l =
  let vs = StrSet.elements (vars l) in
  let make_sub =
    List.fold_left2 (fun s v t -> StrMap.add v t s) StrMap.empty vs in
    iter_permut
      (fun a ->
	 let t = subs (make_sub (Array.to_list a)) l in
	   if List.exists trace_exists_loop_from [t] then raise Exit)
      (Array.of_list
	 (List.map
	    (fun v -> Array.of_list (Db.gen_terms k (typ_of_ident v))) vs));;

let exists_loop() =
  try
    debug "try lhs...";
    List.iter
      (fun (l, _) -> if List.exists trace_exists_loop_from [l]
       then raise Exit)
      (get_rules());
    (*REMOVE:debug "try lhs instances...";
    List.iter
      (fun (l, _) -> check_subs 1 l)
	 (*REMOVE:if List.exists trace_exists_loop_from (gen_subs 1 l)
	 then raise Exit*)
      (get_rules());*)
    debug "try generate terms...";
    StrMap.iter
      (fun f _ ->
	 if List.exists trace_exists_loop_from (Db.gen_terms_hd 3 f)
	 then raise Exit)
      (get_rules_map());
    false
  with Exit -> true;;

(*****************************************************************************)
(** strategy for finding a loop *)
(*****************************************************************************)

let try_prove_non_termin() =
  big_sep(); verbose "try to prove non termination...";
  if not (List.for_all (fun (l, _) -> is_pattern l) (get_rules())) then
    (verbose "don't know how to prove the non-termination of rules \
      with non pattern lhs"; quit Maybe);
  verbose "try to find a loop...";
  if exists_loop() then quit No;;
