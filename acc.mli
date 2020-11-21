(** Accessibility.

HOT, a Higher-Order Termination prover
See the COPYRIGHTS and LICENSE files.

- Frederic Blanqui, 2010-03-18
*)

open Term;;
open Prec;;
open Util;;

(*****************************************************************************)
(** Function building a quasi-ordering on type constants such that,
    for every rule [l->r] and some (but not all) variable [x] freely
    occurring in [r], there is some free occurrence of [x] in [l] that
    is accessible. Accessibility is defined as the reflexive and
    transitive closure of:

    t is strictly accessible in [c t1 .. tn] if there are types T1, ..,
    Tn, a type constant B and an integer i such that t=ti,
    c:T1=>..=>Tn=>B and B occurs only positively in Ti.

    - B occurs only positively in B

    - B occurs only positively (resp. negatively) in T=>U if it occurs
    only negatively (resp. positively) in T and only positively
    (resp. negatively) in U. *)
(*****************************************************************************)

val get_typ_prec : unit -> StrPrec.t;;

val build_typ_prec : ident list list -> unit;;

(*****************************************************************************)
(** functions available once the precedence has been computed *)
(*****************************************************************************)

val typ_cmp : ident -> ident -> comp;;
val tid_eq : ident -> ident -> bool;;
val tid_le : ident -> ident -> bool;;
val tid_lt : ident -> ident -> bool;;

(** tell if all type idents occurring in typ are smaller or equal to
    tid0 and if type idents equivalent to tid0 occurs only
    positively/negatively *)

val is_pos_in_typ : ident -> typ -> bool;;
val is_neg_in_typ : ident -> typ -> bool;;

(** tell if a variable is accessible *)

val is_acc : ident -> terms -> bool;;

(** recursive arguments of a function symbol *)

val rec_args : ident -> terms -> terms;;

(** check whether a type constant is positive. a type constant A is
    positive if, for every constructor c:T1=>..=>Tn=>A, index i and type
    constant B occurring in Ti, either B is smaller than A or B is
    equivalent to A and occurs only positively in Ti *)

val is_pos : ident -> bool;;

(** set of constructors of an equivalence class of types *)

val cons_eq : ident -> Util.StrSet.t;;

(** check whether a type constant is basic. a type constant is basic
    if its equivalence class is basic. an equivalence class C of type
    constants is basic if, for every type constant A in C, constructor
    c:T1=>..=>Tn=>A and index i, either A does not occur positively in
    Ti, or Ti is a type constant and either Ti is in C or Ti is
    smaller than A. *)

val is_basic : ident -> bool;;
val is_basic_typ : typ -> bool;;

(** accessible subterms *)

val is_acc_subterm_eq : typ StrMap.t -> term -> term -> bool;;
