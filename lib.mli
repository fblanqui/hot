(** Basic data types and functions.

HOT, a Higher-Order Termination prover
See the COPYRIGHTS and LICENSE files.

- Frederic Blanqui, 2010-03-15
*)

open Util;;

(*****************************************************************************)
(** useful exception *)
(*****************************************************************************)

exception Found of string;;

val found : string -> 'a;;

(*****************************************************************************)
(** printing messages or debugging information *)
(*****************************************************************************)

val verbose : string -> unit;;
val warning : string -> unit;; (* verbose prefixed by "warning: " *)
val verbosef : string -> 'a bprint -> 'a -> unit;;

val big_sep : unit -> unit;;
val small_sep : unit -> unit;;

val debug : string -> unit;;
val debugf : string -> 'a bprint -> 'a -> unit;;
val debugv : string -> 'a bprint -> 'a -> 'a;;
val debug1 : string -> 'a bprint -> 'b bprint -> ('a -> 'b) -> ('a -> 'b);;
val debug1' : string -> 'a bprint -> 'b bprint -> ('a -> 'b) -> ('a -> 'b);;
val debug2 : string -> 'a1 bprint -> 'a2 bprint -> 'b bprint ->
  ('a1 -> 'a2 -> 'b) -> ('a1 -> 'a2 -> 'b);;
val debug2' : string -> 'a1 bprint -> 'a2 bprint -> 'b bprint ->
  ('a1 -> 'a2 -> 'b) -> ('a1 -> 'a2 -> 'b);;

val print_buffer : unit -> unit;;

(*****************************************************************************)
(** exceptions raised by tactics *)
(*****************************************************************************)

exception Error;;

val error : string -> 'b;;
val errorf : string -> 'a bprint -> 'a -> 'b;;

val try_crit : (unit -> unit) -> unit;;

(*****************************************************************************)
(** exit function *)
(*****************************************************************************)

type result = Yes | No | Maybe;;

val quit : result -> 'a;;

(*****************************************************************************)
(** miscellaneous functions *)
(*****************************************************************************)

val xor : bool -> bool -> bool;;

val is_Some : 'a option -> bool;;

val array : string -> 'a bprint -> 'a array bprint;;

val is_set_get_set :
  string -> 'a -> (unit -> bool) * (unit -> 'a) * ('a -> unit);;

(*****************************************************************************)
(** permutations *)
(*****************************************************************************)

(** given an array of (non-empty) arrays l, next l returns each time a
    new choice of elements x such that x.(i) in l.(i), or raise
    Not_found *)
val next : 'a array array -> unit -> 'a array;;

val iter_permut : ('a array -> unit) -> 'a array array -> unit;;

(*****************************************************************************)
(** iteration functions *)
(*****************************************************************************)

val iter : int -> ('a -> 'a) -> 'a -> 'a;;
val iter_opt : int -> ('a -> 'a option) -> 'a -> 'a;;
val nf : ('a -> 'a option) -> 'a -> 'a;;

(*****************************************************************************)
(** functions on StrSet.t and StrMap.t *)
(*****************************************************************************)

val set_of_list : string list -> StrSet.t;;

val keys : 'a StrMap.t -> StrSet.t;;

(** [adds m1 m2] adds the bindings of m1 in m2 *)
val adds : 'a StrMap.t -> 'a StrMap.t -> 'a StrMap.t;;

val lfind : string -> 'a list StrMap.t -> 'a list;;
val ladd : string -> 'a -> 'a list StrMap.t -> 'a list StrMap.t;;

(*****************************************************************************)
(** temporary files *)
(*****************************************************************************)

val new_temp : (*prefix*)string -> (*postfix*)string -> string;;
val new_temp_change_postfix :
  (*previous temp file name*)string -> (*new postfix*)string -> string;;
val remove_temps : unit -> unit;;

(*****************************************************************************)
(** external tools *)
(*****************************************************************************)

val output_file : (out_channel -> 'a -> unit) -> string -> 'a -> unit;;

val output_file_xtc : string -> Xtc.problem -> unit;;

val command : string -> int;;

val is_set_lp_solver : unit -> bool;;
val get_lp_solver : unit -> string;;
val set_lp_solver : string -> unit;;

(*****************************************************************************)
(** equality on type constructors *)
(*****************************************************************************)

type 'a eq = 'a -> 'a -> bool;;

val option_eq : 'a eq -> 'a option eq;;

(*****************************************************************************)
(** functions for managing tests *)
(*****************************************************************************)

val get_test_mode : unit -> bool;;
val set_test_mode : unit -> unit;;

val add_test : string -> int -> bool -> unit;;
val print_tests : unit -> unit;;

(*****************************************************************************)
(** functions on lists *)
(*****************************************************************************)

val ml_list : 'a bprint -> 'a list bprint;;

(** intersection of two lists *)
val inter : 'a list -> 'a list -> 'a list;;

(** variant of List.exists where the function takes also the position
    as argument *)
val exists_i : (int -> 'a -> bool) -> 'a list -> bool;;

(** return a pair made of the list of the k first elements and the
    list of remaining elements. raise Invalid_arg if there are less
    than k elements. *)
val split : int -> 'a list -> 'a list * 'a list;;

(** variant of List.exists checking for two elements satisfying f *)
val exists2 : ('a -> bool) -> 'a list -> bool;;

(** given a list of lists l1 .. ln, compute the list of all the lists
    l which 1st element is in l1, 2nd element is in l2, etc. *)
val enum : 'a list list -> 'a list list;;

(** variant of List.fold_left taking the index of the element as argument *)
val fold_left_i : ('a -> int -> 'b -> 'a) -> 'a -> 'b list -> 'a;;

(** [find_Some f [x1;..;xn]] returns [f xi] if i is the smallest index
    such that [f xi] <> None. It returns None otherwise. It is an
    optimization of [List.find ((<>) None) (List.map f xs)]. *)
val find_Some : ('a -> 'b option) -> 'a list -> 'b option;;

(** [fold_Some f a [x1;..;xn]] returns [fst (f ai xi)] if i is the
    smallest index such that [fst (f ai xi)] <> None, a1=a,
    a_{i+1}=snd(f ai xi). It returns None otherwise. It is an
    optimization of [List.find ((<>) None) (fst (List.fold_left (fun
    (l, a) xi -> let v, a' = f a xi in (v :: l, a')) ([], a) xs))]. *)
val fold_Some : ('a -> 'b -> 'c option * 'a) -> 'a -> 'b list -> 'c option;;

(** extension to ['a list -> 'b list] of a function [f:'a -> 'b list] *)
val fold_app : ('a -> 'b list) -> 'a list -> 'b list;;

(*****************************************************************************)
(** iteration functions on two lists *)
(*****************************************************************************)

(** variation of List.fold_left2 raising invalid_arg only if the first
    list is smaller then the second list *)
val fold_left2 : ('a -> 'b -> 'c -> 'a) -> 'a -> 'b list -> 'c list -> 'a;;

(** variation of List.iter2 raising invalid_arg only if the first list
    is smaller then the second list *)
val iter2 : ('b -> 'c -> unit) -> 'b list -> 'c list -> unit;;

(** filter the second list by doing the tests with the first list.
    raise invalid_arg if the first list is smaller than the second list *)
val filter2 : ('a -> bool) -> 'a list -> 'b list -> 'b list;;

(** return the first element of second list such that the
    corresponding element of the first list satisfies the required
    condition. raise Not_found if there is no such element. raise
    invalid_arg if the first list is smaller than the second list *)
val first2 : ('a -> bool) -> 'a list -> 'b list -> 'b;;

(** variation of List.for_all2 raising invalid_arg only if the first
    list is smaller then the second list *)
val for_all2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool;;

(*****************************************************************************)
(** printable types *)
(*****************************************************************************)

module type PRN = sig
  type t
  val print : t bprint
end;;

module StrPrn : PRN with type t = string;;

(*****************************************************************************)
(** memorization functions *)
(*****************************************************************************)

module type MEM = sig
  type key
  val memo : (key -> 'a) -> (key -> 'a)
  val memo_debugf : string -> 'a bprint -> (key -> 'a) -> (key -> 'a)
end;;

module Mem : sig
  module Make (S : Map.S) (P : PRN with type t = S.key)
    : MEM with type key = S.key
end;;

module StrMem : MEM with type key = string;;
module IntMem : MEM with type key = int;;

(*****************************************************************************)
(** Getting fresh names.  Use [notin vs] to generate names not in
    [vs]. Each call to [fresh] generates a new name not belonging to
    the sets declared with [notin] and different from previous calls
    to [fresh]. *)
(*****************************************************************************)

val notin : StrSet.t -> unit;;
val fresh : string -> string;;
