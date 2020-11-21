(**
HOT, a Higher-Order Termination prover
See the COPYRIGHTS and LICENSE files.

- Frederic Blanqui, 2010-03-18
*)

open Term;;
open Expr;;
open Prec;;
open Util;;
open Lib;;
open Printf;;

(*****************************************************************************)
(** Type ordering constraints necessary for a type constant to occur
    only positively (resp. negatively) in a type.

    May raise the exception Incompatible. *)
(*****************************************************************************)

(* extends [prec] for [id] to occur only positively in [typs, id'] *)
let rec prec_pos id prec (typs, id') =
  (* [id] must occur only negatively in every element of [typs] *)
  List.fold_left
    (prec_neg id)
    (StrPrec.add id' Le id prec) (* and we must have [id' <= id] *)
    typs

(* extends [prec] for [id] to occur only negatively in [typs, id'] *)
and prec_neg id prec (typs, id') =
  (* [id] must occur only positively in every element of [typs] *)
  List.fold_left
    (prec_pos id)
    (StrPrec.add id' Lt id prec) (* and we must have [id' < id] *)
    typs;;

(*****************************************************************************)
(** "Occurrences" of a variable in a pattern.

    Usually, an occurrence of a variable in a term is a list of
    non-negative integers describing the path to follow to reach the
    variable from the top of the term, an integer i meaning that we go
    through the i-th argument of a function symbol. For instance, in
    the term [fx(gx)], x has for occurrences [0] and [1;0].

    Here, we also specify the function symbols and do not take into
    account abstractions. Hence, in [f(\y.x)(gx)], x has for
    occurrences ["f",0] and ["f",1;"g",0].

    Occurrences of variables not applied to distinct bound variables
    are not taken into account. *)
(*****************************************************************************)

(*IMPROVE: x is accessible in \y.yx *)

type occ = (ident * int) list;;

let occ b = bprintf b "[%a]" (list "; " (pair string ", " int));;

module Occ = struct type t = occ end;;
module OccOrd = Ord.Make (Occ);;
module OccSet = Set.Make (OccOrd);;

let occs b s = bprintf b "{%a}" (list ",\n" occ) (OccSet.elements s);;

let rec occs_term_aux occset prefix bv x (h, ts) =
  match h with
    | Symb (Fun id) -> occs_terms_aux occset prefix bv id 0 x ts
    | Symb (Var id) ->
	if id = x (*&& not (StrSet.mem id bv)*)
	  (*this test can be removed because it is already done in Abs case *)
	  && are_distinct_bound_vars bv ts then
	    OccSet.add (List.rev prefix) occset
	else occset
    | Abs (idtyps, t) ->
	if List.exists (fun (id, _) -> id = x) idtyps then occset
	else occs_term_aux occset prefix (add_vars bv idtyps) x t

and occs_terms_aux occset prefix bv id n x = function
  | [] -> occset
  | t :: ts ->
      occs_terms_aux
	(occs_term_aux occset ((id, n) :: prefix) bv x t)
	prefix bv id (n+1) x ts;;

let occs_term = occs_term_aux OccSet.empty [] StrSet.empty;;

let occs_terms x =
  List.fold_left
    (fun occset t -> occs_term_aux occset [] StrSet.empty x t)
    OccSet.empty;;

(*****************************************************************************)
(** Precedence associated to an occurrence:

    if we need to go through the [k]-th argument of a function symbol
    [id] of type [typs => typid] to reach a variable, then [typid]
    must occur only positively in [List.nth typs k].

    May raise the exception Incompatible. *)
(*****************************************************************************)

let rec prec_occ_aux prec = function
  | [] -> prec
  | (id, k) :: occs ->
      (*debug ("id = " ^ id);*)
      prec_occ_aux
	(let typs, typid = typ_of_ident id in
	   prec_pos typid prec (List.nth typs k))
	occs;;

let prec_occ = prec_occ_aux StrPrec.empty;;

(*****************************************************************************)
(** Finding a precedence so that, for every rule [l->r], if a variable
    [x] occurs freely in [r] then there is an occurrence of [x] in [l]
    that is accessible. *)
(*****************************************************************************)

(** Module for building sets/disjunctions of precedences (precedences
   are themselves conjonctions of ordering constraints). Each
   occurrence of a variable gives a set of constraints and there may
   be various occurrences. *)
module StrPrecSet = Set.Make (StrPrec);;

(** Add into the set of precedences [precs] the precedence [prec_occ
   occ] if it is satisfiable (does not raise Incompatible). *)
let add occ precs =
  try StrPrecSet.add (prec_occ occ) precs with Incompatible -> precs;;

(** In the following, [prec] is a precedence and [precs_list] is a
    conjonction of sets/disjunctions of precedences. Indeed, we must
    find a quasi-ordering that works for all the rules and, for every
    rule, there might be various possibilities depending on which
    variable occurrences are used. The constraints that must be
    satisfied whatever the occurrence is (especially if there is only
    one possible occurrence) are put in [prec].

    precs_list = /\_i precs_i, and precs_i = \/_j prec_{i,j} *)

let add_rhs_vars_acc (prec, precs_list) (((h, ls), r) as lr) =
  match h with
    | Symb (Var _) | Abs _ -> prec, precs_list
    | Symb (Fun _) ->
	(*IMPROVE: exclude rules of the form f .. r .. -> r *)
	if List.mem r ls then prec, precs_list
	else
	  fold_vars (* for each free RHS variable [x] *)
	    (fun x _ts (prec, precs_list) ->
	       (* satisfiable precedences corresponding to the
		  occurrences of [x] in [ls] *)
	       let precs = OccSet.fold add (occs_terms x ls) StrPrecSet.empty in
		 match StrPrecSet.cardinal precs with
		   | 0 ->
		       verbosef ("remark: " ^ x ^ " is not accessible in ")
			 rule lr;
		       prec, precs_list
		   | 1 ->
		       (try StrPrec.merge (StrPrecSet.choose precs) prec
			with Prec.Incompatible -> prec), precs_list
		   | _ -> prec, precs :: precs_list)
	    (prec, precs_list)
	    r;;

(*****************************************************************************)
(** merge two precedences if possible *)
(*****************************************************************************)

let merge_opt prec' prec =
  try Some (StrPrec.merge prec' prec)
  with Prec.Incompatible -> None;;

(*****************************************************************************)
(** check the satisfiability of [prec /\ precs_list] *)
(*****************************************************************************)

exception Check_sat of StrPrec.t;;

let rec check_sat prec = function
  | [] -> raise (Check_sat prec)
  | precs :: precs_list ->
      StrPrecSet.iter
	(fun prec' ->
	   match merge_opt prec' prec with
	     | None -> ()
	     | Some new_prec -> check_sat new_prec precs_list)
	precs;;

let check_sat_opt prec precs_list =
  try check_sat prec precs_list; None
  with Check_sat prec -> Some prec;;

(*****************************************************************************)
(** build the precedence on type constants *)
(*****************************************************************************)

let get_typ_prec, set_typ_prec = get_set StrPrec.empty;;

(* don't remove arguments! *)
let typ_cmp tid1 tid2 = StrPrec.cmp (get_typ_prec()) tid1 tid2;;
let tid_eq tid1 tid2 = StrPrec.eq (get_typ_prec()) tid1 tid2;;
let tid_le tid1 tid2 = StrPrec.le (get_typ_prec()) tid1 tid2;;
let tid_lt tid1 tid2 = StrPrec.lt (get_typ_prec()) tid1 tid2;;

(*let typ_cmp = debug2 "typ_cmp " string string comp typ_cmp;;*)

let rec nb_sols_max = function
  | [] -> 1
  | precs :: precs_list -> StrPrecSet.cardinal precs * nb_sols_max precs_list;;

let check_typ_prec (prec, precs_list) =
  verbosef "number of solutions <= " int (nb_sols_max precs_list);
  let prec =
    match check_sat_opt prec precs_list with
      | None -> prec
      | Some prec -> prec in
  let prec = StrPrec.strict prec in
    set_typ_prec prec;
    verbosef "precedence:\n" (StrPrec.prec string) prec;;

let build_typ_prec l =
  verbose "build quasi-ordering on types...";
  check_typ_prec
    (List.fold_left
       (List.fold_left
	  (fun x f -> List.fold_left add_rhs_vars_acc x (rules_of_ident f)))
       (StrPrec.empty, [])
       l);;

(*****************************************************************************)
(** tell if all type idents occurring in typ are smaller or equal to
    tid0 and if type idents equivalent to tid0 occurs only
    positively/negatively *)
(*****************************************************************************)

let rec is_pos_in_typ tid0 (typs, tid) =
  tid_le tid tid0 && List.for_all (is_neg_in_typ tid0) typs

and is_neg_in_typ tid0 (typs, tid) =
  tid_lt tid tid0 && List.for_all (is_pos_in_typ tid0) typs;;

(*let is_pos_in_typ = debug2 "is_pos_in_typ " string typ bool is_pos_in_typ;;*)

(*****************************************************************************)
(** tell if a variable is accessible *)
(*****************************************************************************)

let rec is_pos_occ = function
  | [] -> true
  | (id, k) :: occ ->
      let typs, tid = typ_of_ident id in
	is_pos_in_typ tid (List.nth typs k) && is_pos_occ occ;;

let is_acc x ls = OccSet.exists is_pos_occ (occs_terms x ls);;

(*****************************************************************************)
(** recursive arguments of a function symbol *)
(*****************************************************************************)

let rec_args_filter =
  let aux cid =
    let typs, tid = typ_of_ident cid in List.map (is_pos_in_typ tid) typs
  in StrMem.memo(*_debugf "rec_args_filter" (ml_list bool)*) aux;; 

let rec_args cid = Lib.filter2 (fun x -> x) (rec_args_filter cid);;

(*****************************************************************************)
(** Check whether a type constant is positive. A type constant A is
    positive if, for every constructor c:T1=>..=>Tn=>A, index i and type
    constant B occurring in Ti, either B is smaller than A or B is
    equivalent to A and occurs only positively in Ti *)
(*****************************************************************************)

let is_pos =
  let aux tid =
    StrSet.for_all (* constructor *)
      (fun cid -> let typs, _ = typ_of_ident cid in
	 List.for_all (is_pos_in_typ tid) typs)
      (cons tid)
  in StrMem.memo(*_debugf "is_pos" bool*) aux;;

(*****************************************************************************)
(** set of constructors of an equivalence class of types *)
(*****************************************************************************)

let cons_eq tid0 =
  StrMap.fold
    (fun tid tidcs cs -> if tid_eq tid tid0 then StrSet.union tidcs cs else cs)
    (get_cons_map())
    StrSet.empty;;

(*****************************************************************************)
(** Check whether a type constant is basic. A type constant is basic
    if its equivalence class is basic. An equivalence class C of type
    constants is basic if, for every type constant A in C, constructor
    c:T1=>..=>Tn=>A and index i, either A does not occur positively in
    Ti, or Ti is a type constant and either Ti is in C or Ti is
    smaller than A and basic. *)
(*****************************************************************************)

let rec is_basic =
  let memo = ref StrMap.empty in
  let rec is_basic tid =
    StrSet.for_all
      (fun cid ->
	 let typs, _ = typ_of_ident cid in
	   List.for_all
	     (fun ((typs', tid') as typ') ->
		not (is_pos_in_typ tid typ')
		|| (typs' = []
		    && match typ_cmp tid' tid with
		      | Eq -> true
		      | Lt | Le -> is_basic_memo tid'
		      | Un | Ge | Gt -> false))
	     typs)
      (cons_eq tid)
  and is_basic_memo tid =
    try StrMap.find tid !memo
    with Not_found ->
      let b = is_basic tid in
	(*debug (sprintf "is_basic(%s)=%b" tid b);*)
	memo := StrMap.add tid b !memo; b
  in is_basic_memo;;

let is_basic_typ (typs, tid) = typs = [] && is_basic tid;;

(*****************************************************************************)
(** accessible subterms *)
(*****************************************************************************)

let is_acc_subterm_eq vm r =

  let rec aux bv l =
    (*debugf "is_acc_subterm_eq_aux "
      (pair (set ",") "; " term) (bv, l);*)
    let ((h, us) as l) = remove_distinct_bound_vars bv l in
      alpha_eq r l
      || match h with
	| Symb (Fun f) -> List.exists (aux bv) (rec_args f us)
	| Symb (Var x) -> StrSet.mem x bv && List.exists (aux bv) us
	| Abs ([], t) -> aux bv t
	| Abs ((id, _) :: idtyps', t) ->
	    us = [] && aux_abs (StrSet.add id bv) t idtyps'

  and aux_abs bv t = function
    | [] -> aux bv t
    | ((id, _) :: idtyps') as idtyps ->
	alpha_eq r (Abs (idtyps, t), [])
	|| aux_abs (StrSet.add id bv) t idtyps'

  in fun l ->
    aux StrSet.empty l
    || (is_subterm_eq r l && is_basic_typ (typ_term vm r)
	&& StrSet.subset (vars r) (vars l));;

(*let is_acc_subterm_eq vm =
  (*debugf "is_acc_subterm_eq " (pair pterm " " pterm) (r, l);*)
  debug2 "is_acc_subterm_eq " pterm pterm bool (is_acc_subterm_eq vm);;*)
