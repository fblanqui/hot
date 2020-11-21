(** First-order symbols and systems.

HOT, a Higher-Order Termination prover
See the COPYRIGHTS and LICENSE files.

- Frederic Blanqui, 2010-03-15
*)

open Term;;
open Lib;;
open Util;;

(** the set FO of first-order function symbols if the biggest subset
    of function symbols having a first-order type and in the defining
    rules of which there is no abstraction, no applied variables and only
    function symbols of FO *)

val compute_fo_funs : unit -> unit;;
val get_fo_funs : unit -> StrSet.t;;

(** calling an external first-order prover *)

val is_set_fo_prover : unit -> bool;;
val get_fo_prover : unit -> string;;
val set_fo_prover : string -> unit;;

type fo_problem = int StrMap.t * rules;;

val call_fo_prover : string -> fo_problem -> result;;

(** try to prove the termination of the FO part *)

val try_prove_fo_part : unit -> unit;;
