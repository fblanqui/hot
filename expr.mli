(** Convert raw lambda expressions into terms and build various maps.

HOT, a Higher-Order Termination prover
See the COPYRIGHTS and LICENSE files.

- Frederic Blanqui, 2010-03-15

This file provides:

    - A type for raw lambda expressions (when we don't know whether an
    identifier is a variable or a function symbol) and functions for
    building terms from expressions.

    - Types and printing functions for termination problems.

    - A function computing the sets of defined and undefined function
    symbols wrt a set of rules.

    - A function computing the definition (quasi-)ordering on defined
    symbols: f >= g if g is a defined symbol occuring in some defining
    rule of f.

    - A function computing the sets of constructor of each type.
*)

open Term;;
open Util;;
open Prec;;

(*****************************************************************************)
(** raw lambda expressions provided by parsing *)
(*****************************************************************************)

type expr =
  | EIdent of ident
  | EAbs of ident * typ * expr
  | EApp of expr * expr;;

val expr_app : expr -> expr list -> expr;;

val raw_expr : expr bprint;; (* with parentheses around applications *)
val expr : expr bprint;; (* without parentheses around applications *)

type expr_rule = expr * expr;;
type expr_rules = expr_rule list;;

val expr_rule : expr_rule bprint;;

(*****************************************************************************)
(** signature and termination problems *)
(*****************************************************************************)

type signature = {
  sig_typs : StrSet.t;
  sig_funs : typing;
  sig_vars : typing };;

type expr_problem = signature * expr_rules;;
type problem = signature * rules;;

val expr_problem : expr_problem bprint;;
val problem : problem bprint;;

val output_file_hot : string -> problem -> unit;;

(*****************************************************************************)
(** set the map ident -> symbol * typ for converting raw lambda
    expressions into terms *)
(*****************************************************************************)

val build_symb_typ_map : signature -> unit;;
val get_symb_typ_map : unit -> (symb * typ) StrMap.t;;

(*****************************************************************************)
(** type-checking *)
(*****************************************************************************)

val add_global_symb : symb -> typ -> unit;;
val add_global_fresh : typ -> term;;
val add_global_freshs_typ : typ -> int -> terms;;
val typ_of_ident : ident -> typ;;
val typs_of_idents : StrSet.t -> typing;;
val arity_fun : ident -> int;;
val typ_term : typing -> term -> typ;;
val typ_of : term -> typ;;
val preserve_typ_rules : rules -> bool;;

(*****************************************************************************)
(** type constants necessary for typing a term *)
(*****************************************************************************)

val typ_consts : term -> StrSet.t;;
val typ_consts_rule : rule -> StrSet.t;;
val typ_consts_rules : rules -> StrSet.t;;

(*****************************************************************************)
(** set the set of rules so that bound variables are pairwise distinct
    and distinct from free variables *)
(*****************************************************************************)

val compute_rules : expr_rules -> unit;;
val get_rules : unit -> rules;;

(*****************************************************************************)
(** set of symbols the termination of which remains to be proved *)
(*****************************************************************************)

val get_funs : unit -> StrSet.t;;
val set_funs : StrSet.t -> unit;;

(*****************************************************************************)
(** symbols that have been proved computable *)
(*****************************************************************************)

val get_comp_funs : unit -> StrSet.t;;
val add_comp_fun : ident -> unit;;
val add_comp_funs : StrSet.t -> unit;;
val is_comp : ident -> bool;;

(*****************************************************************************)
(** useful data structures available after calling [build_maps] hereafter *)
(*****************************************************************************)

val build_maps : unit -> unit;;

(** set of type constants *)
val get_typ_consts : unit -> StrSet.t;;

(** map (function) ident -> rules *)
val get_rules_map : unit -> rules StrMap.t;;
val is_defined : ident -> bool;;
val rules_of_ident : ident -> rules;;
val rules_of_idents : StrSet.t -> rules;;

(** non symbol headed rules *)
val get_other_rules : unit -> rules;;

(** undefined symbols *)
val get_undef_funs : unit -> StrSet.t;;

(** definition (quasi-)ordering on defined symbols: f >= g if g is a
    defined symbol occuring in some defining rule of f not of the form
    f .. (g ls) .. -> g ls. [def_cmp] only returns Eq, Gt, Lt or Un. *)
val get_def_prec : unit -> StrPrec.t;;
val def_cmp : ident -> ident -> Prec.comp;;
val funs_le : StrSet.t -> StrSet.t;;

(** tell if a function symbol is recursively defined *)
val is_rec : ident -> bool;;

(** map (typ) ident -> (constructor) idents. by constructor, we mean
    all the function symbols used in the LHS of a rule but the top
    symbol *)
val get_cons_map : unit -> StrSet.t StrMap.t;;
val cons : ident -> StrSet.t;;
val is_cons : ident -> bool;;

(** map tid -> list of pairs (f, tys) such that typ_of_ident f = tys, tid *)
val get_typ_map : unit -> (ident * typ list) list StrMap.t;;
