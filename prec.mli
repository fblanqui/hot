(** Finite quasi-orderings.

HOT, a Higher-Order Termination prover
See the COPYRIGHTS and LICENSE files.

- Frederic Blanqui, 2010-03-17

   This module provides a type for incrementally building quasi-orderings
   from ordering constraints. In particular, it computes the
   reflexive, transitive and symmetric closures of the constraints. It
   raises the exception [Incompatible] when the addition of a new
   constraint leads to an unsatisfiable set of constraints.
*)

open Lib;;
open Util;;

(*****************************************************************************)
(** type for ordering constraints *)
(*****************************************************************************)

type comp = Le | Lt | Eq | Gt | Ge | Un;;

val string_of_comp : comp -> string;;

val comp : comp bprint;;

val opp : comp -> comp;;

(*****************************************************************************)
(** exception raised when adding a new constraint leads to an
    unsatisfiable set of constraints *)
(*****************************************************************************)

exception Incompatible;;

(*****************************************************************************)
(** signature describing the provided functionalities *)
(*****************************************************************************)

module type S = sig

  type elt
  module EltMap : Map.S with type key = elt
  module EltSet : Set.S with type elt = elt

  (** Sets of constraints are represented by using finite maps of type
      [elt -> elt -> comp] such that the sets of pairs [(x,y)] such that
      - [comp prec x y = Eq] is an equivalence relation
      - [comp prec x y = Ge|Gt|Le|Lt] is a quasi-ordering

      In addition, [comp prec x y = opp (comp prec y x)] *)

  type t = comp EltMap.t EltMap.t

  val cmp : t -> elt -> elt -> comp
  val le : t -> elt -> elt -> bool
  val lt : t -> elt -> elt -> bool
  val eq : t -> elt -> elt -> bool

  val find : elt -> t -> comp EltMap.t
  val fold : (elt -> elt -> comp -> 'b -> 'b) -> t -> 'b -> 'b
  val iter : (elt -> elt -> comp -> unit) -> t -> unit
  val fold_elt : elt -> (elt -> comp -> 'b -> 'b) -> t -> 'b -> 'b

  (** elements [y] such that [comp x y = c] *)
  val elts : t -> elt -> comp -> EltSet.t

  val prec : elt bprint -> t bprint

  val empty : t
  val add : elt -> comp (* <> Un *) -> elt -> t -> t
  val add_elt : elt -> t -> t

  val merge : t -> t -> t

  (** total ordering on t *)
  val compare : t -> t -> int

  (** replace Ge by Gt and Le by Lt *)
  val strict : t -> t

  (** replace Ge and Le by Eq *)
  val large : t -> t

  (** list of equivalence classes in increasing order *)
  val linear : t -> elt list list

  val filter : (elt -> bool) -> t -> t

end;;

(*****************************************************************************)
(** functor providing the above functionalities *)
(*****************************************************************************)

module Make (O : ORD) (OM : Map.S with type key = O.t)
  (OS : Set.S with type elt = O.t) (OP : PRN with type t = O.t) : S
  with type elt = O.t
  with module EltMap = OM
  with module EltSet = OS
  with type t = comp OM.t OM.t;;

(*****************************************************************************)
(** finite quasi-orderings on strings *)
(*****************************************************************************)

module StrPrec : S
  with type elt = string
  with module EltMap = StrMap
  with module EltSet = StrSet
  with type t = comp StrMap.t StrMap.t;;
