(** Call matrices.

HOT, a Higher-Order Termination prover
See the COPYRIGHTS and LICENSE files.

- Frederic Blanqui, 2011-09-28
*)

open Util;;
open Matrix;;

(** relation on arguments *)
type rel = Lt | Eq | Un;;

val rel : rel bprint;;

(** call matrices *)
module CM : TYP with type t = string * string * rel matrix;;
module CMOrd : ORD with type t = CM.t;;
module CMSet : Set.S with type elt = CM.t;;

val call_matrices : CMSet.t bprint;;

val completion : CMSet.t -> CMSet.t;;

val lex_orders : CMSet.t -> int list StrMap.t;;
