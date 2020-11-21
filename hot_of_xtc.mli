(** Convert XTC into HOT.

HOT, a Higher-Order Termination prover
See the COPYRIGHTS and LICENSE files.

- Frederic Blanqui, 2010-09-15
*)

val get_arity : Term.ident -> int;;

val expr_problem_of_xtc_problem : Xtc.problem -> Expr.expr_problem;;
