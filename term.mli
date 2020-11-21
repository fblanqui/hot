(** Type and functions on terms.

HOT, a Higher-Order Termination prover
See the COPYRIGHTS and LICENSE files.

- Frederic Blanqui, 2010-03-15
*)

open Util;;

(*****************************************************************************)
(** identifiers *)
(*****************************************************************************)

type ident = string;;

(*****************************************************************************)
(** simple types *)
(*****************************************************************************)

type typ = typ list * ident;;
(** ([t1; ..; tn], id) represents t1 => .. => tn => id *)

type typing = typ StrMap.t;;

val typ_const : ident -> typ;;
val arrow : typ -> typ -> typ;;
val arrows : typ list -> typ -> typ;;

val typ : typ bprint;;
val ptyp : typ bprint;;

val arity_typ : typ -> int;;

val is_base_typ : typ -> bool;;

val iter_typ : (ident -> unit) -> typ -> unit;;
val fold_typ : (ident -> 'a -> 'a) -> 'a -> typ -> 'a;;

val exists_typ : (ident -> bool) -> typ -> bool;;
val exists_typs : (ident -> bool) -> typ list -> bool;;

val in_typ : ident -> typ -> bool;;
val in_typs : ident -> typ list -> bool;;

(*****************************************************************************)
(** well-formed lambda terms *)
(*****************************************************************************)

type symb = Var of ident | Fun of ident;;

val ident_of_symb : symb -> ident;;

type term = head * terms
  (** (h, [t1; ..; tn]) represents the application (h t1 .. tn) *)
and head = Symb of symb | Abs of (ident * typ) list * term
  (** Abs ([x1,t1; .., xn,tn], u) represents \x1:t1. .. \xn:tn. u *)
and terms = term list;;

val symb : symb -> term;;
val var : ident -> term;;
val func : ident -> term;;
val app : term -> term -> term;;
val apps : term -> terms -> term;;
val appfunc : ident -> terms -> term;;
val abs1 : ident -> typ -> term -> term;;
val abs : (ident * typ) list -> term -> term;;

val term : term bprint;;
val pterm : term bprint;;

(* for debug *)
val raw_term : term bprint;;
val praw_term : term bprint;;

val fresh_vars : ident -> int -> terms;;
val fresh_idtyps : ident -> typ list -> (ident * typ) list;;

(** iterators on function symbols and free variables *)
val fold_funs : (ident -> terms -> 'b -> 'b) -> 'b -> term -> 'b;;
val fold_vars : (ident -> terms -> 'b -> 'b) -> 'b -> term -> 'b;;
val iter_funs : (ident -> terms -> unit) -> term -> unit;;
val iter_vars : (ident -> terms -> unit) -> term -> unit;;

type rule = term * term;;
type rules = rule list;;

val rule_gen : 'a bprint -> ('a * 'a) bprint;;
val rule : rule bprint;;

(*****************************************************************************)
(** symbols occuring in a term *)
(*****************************************************************************)

val funs : term -> StrSet.t;;
val funs_terms : terms -> StrSet.t;;
val funs_rule : rule -> StrSet.t;;
val funs_rules : rules -> StrSet.t;;

val exists_fun : (ident -> terms -> bool) -> term -> bool;;
val in_funs : ident -> term -> bool;;

(*****************************************************************************)
(** free variables occuring in a term *)
(*****************************************************************************)

val vars : term -> StrSet.t;;
val vars_terms : terms -> StrSet.t;;
val vars_rule : rule -> StrSet.t;;
val vars_rules : rules -> StrSet.t;;

val in_vars : ident -> term -> bool;;

(** number of free variable occurences *)
val nb_occs : ident -> int StrMap.t -> int;;
val occs : term -> int StrMap.t;;

(*****************************************************************************)
(** eta normal form *)
(*****************************************************************************)

val eta : term -> term;;

(*****************************************************************************)
(** bound variables occurring in a term *)
(*****************************************************************************)

val add_vars : StrSet.t -> (ident * typ) list -> StrSet.t;;
val add_var_typs : typing -> (ident * typ) list -> typing;;

val is_bound_var : typing -> term -> bool;;
val are_distinct_bound_vars : StrSet.t -> terms -> bool;;

(** remove ending distinct bound variables *)
val remove_distinct_bound_vars : StrSet.t -> term -> term;;

(*****************************************************************************)
(** alpha equivalence *)
(*****************************************************************************)

type renaming = string StrMap.t;;

val alpha_eq_bv : renaming -> term -> term -> bool;;
val alpha_eq : term -> term -> bool;;

(** rename bound variables by fresh variables *)
val rename_bound_vars : term -> term;;

(*****************************************************************************)
(** substitutions *)
(*****************************************************************************)

type subs = term StrMap.t;;

val bsubs : subs bprint;;

val subs : subs -> term -> term;;

(** extend s by mapping vid to t. raise Exit if s maps vid to a term
    not alpha equivalent to t *)
val add_subs : ident -> term -> subs -> subs;;

(*****************************************************************************)
(** subterm relation and iterators taking bound variables into account *)
(*****************************************************************************)

type 'b bound_vars = 'b * (ident -> typ -> 'b -> 'b);;

val bound_vars_renaming : renaming bound_vars;;
val bound_vars_typing : typing bound_vars;;
val bound_vars : (renaming * typing) bound_vars;;
val bound_vars_unit : unit bound_vars;;

val fold_subterms : 'b bound_vars ->
  ('a -> 'b -> term -> 'a) -> 'a -> term -> 'a;;
val iter_subterms : 'b bound_vars -> ('b -> term -> unit) -> term -> unit;;
val exists_subterm : 'b bound_vars -> ('b -> term -> bool) -> term -> bool;;

val is_subterm_eq : term -> term -> bool;;
val is_subterm : term -> term -> bool;;

(*****************************************************************************)
(** matching modulo alpha-equivalence *)
(*****************************************************************************)

(** [matching l t] returns [Some s] if [alpha_eq (subs s l) t], and
    None otherwise *)
val matching :
  (typing -> term -> typ) -> typing -> term -> term -> subs option;;

(*****************************************************************************)
(** contexts and subterm iterators taking bound variables and contexts
    into account *)
(*****************************************************************************)

type context =
  | Cempty
  | CAbs of ident * typ * context
  | CAppRight of context * term
  | CAppMiddle of head * terms * context * terms;;

val fill : term -> context -> term;;

val term_of_context : context -> term;;

val context : context bprint;;
val pcontext : context bprint;;

val fold_subterms_cont : 'b bound_vars ->
  ('a -> 'b -> context -> term -> 'a) -> 'a -> term -> 'a;;
val iter_subterms_cont : 'b bound_vars ->
  ('b -> context -> term -> unit) -> term -> unit;;
val exists_subterm_cont : 'b bound_vars ->
  ('b -> context -> term -> bool) -> term -> bool;;

(*****************************************************************************)
(** top beta-reduction and rewriting (assuming that rhs variables are
    included in lhs variables) *)
(*****************************************************************************)

type 'a reduct = typing -> context -> 'a -> 'a option;;

val union : 'a reduct -> 'a reduct -> 'a reduct;;

val beta_hd : term reduct;;
val rewrite_hd : (typing -> term -> typ) -> rules -> term reduct;;

type rewrite =
  | Beta
  | Rewrite of rule;;

val rewrite : rewrite bprint;;

type trace = (term * context * rewrite) list;;

val trace : trace bprint;;
val loop : (term * trace) bprint;;

val trace_beta_hd : (term * trace) reduct;;
val trace_rewrite_hd :
  (typing -> term -> typ) -> rules -> (term * trace) reduct;;

(*****************************************************************************)
(** reduct(s) of a term at any position given a function saying if
    there is a reduct at the top *)
(*****************************************************************************)

val reduct : term reduct -> term -> term option;;

type 'a reducts = 'a reduct -> 'a -> 'a list;;
 
val reducts : term reducts;;
val trace_reducts : (term * trace) reducts;;

(*****************************************************************************)
(** useful functions on terms and rules *)
(*****************************************************************************)

(** check if a term is algebraic *)
val is_alg : term -> bool;;

(** check if a term is an higher-order pattern *)
val is_pattern : term -> bool;;

(** check whether a rule is non-duplicating *)
val is_non_dup : rule -> bool;;

(** mgu of (a list of pairs of) algebraic terms *)
val mgu : term -> term -> subs option;;
val mgus : (term * term) list -> subs option;;

(** check whether a left-algebraic system is orthogonal *)
val is_orthogonal_alg : rules -> bool;;
