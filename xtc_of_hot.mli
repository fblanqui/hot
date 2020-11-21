(** Convert HOT into XTC.

HOT, a Higher-Order Termination prover
See the COPYRIGHTS and LICENSE files.

- Frederic Blanqui, 2010-09-15
*)

(** convert a HOT problem into a HO XTC problem with terms in Curry
    form (all function symbols are of arity 0) *)

val cproblem : Expr.problem -> Xtc.problem;;

(** convert a HOT problem into a HO XTC problem with function symbols
    maximally applied *)

val aproblem : Expr.problem -> Xtc.problem;;

(** convert a FO HOT problem into a FO XTC problem *)

val fo_problem : int Util.StrMap.t * Term.rules -> Xtc.problem;;
