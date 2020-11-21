(** Ordering combinators.

HOT, a Higher-Order Termination prover
See the COPYRIGHTS and LICENSE files.

- Frederic Blanqui, 2010-03-22
*)

type 'a cmp = 'a -> 'a -> int;;

val lex : 'a cmp -> 'a list cmp;;
val mul : 'a cmp -> 'a list cmp;;
