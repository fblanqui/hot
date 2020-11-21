(** Matrices.

HOT, a Higher-Order Termination prover
See the COPYRIGHTS and LICENSE files.

- Frederic Blanqui, 2011-09-29
*)

open Util;;

type 'a matrix = private 'a array array;;

val make : int -> int -> 'a -> 'a matrix;;
val init : int -> int -> (int -> int -> 'a) -> 'a matrix;;

val size : 'a matrix -> int * int;;
val nb_lines : 'a matrix -> int;;
val nb_cols : 'a matrix -> int;;

val get : 'a matrix -> int -> int -> 'a;;
val set : 'a matrix -> int -> int -> 'a -> unit;;
 
val iter : ('a -> unit) -> 'a matrix -> unit;;
val iteri : (int -> int -> 'a -> unit) -> 'a matrix -> unit;;

val matrix : 'a bprint -> 'a matrix bprint;;

exception Incompatible;;

val mul : 'a (*zero*) -> ('a -> 'a -> 'a) (*addition*) ->
  ('a -> 'a -> 'a) (*multiplication*) -> 'a matrix -> 'a matrix -> 'a matrix;;

val diag : 'a matrix -> 'a array;;
