(** De Bruijn terms.

HOT, a Higher-Order Termination prover
See the COPYRIGHTS and LICENSE files.

- Frederic Blanqui, 2011-10-28
*)

open Util;;
open Term;;

type db = db_head * db list
and db_head = Db_symb of db_symb | Db_abs of typ list * db
and db_symb = Db_fun of ident | Db_var of int (* >= 0 *);;

module Db : TYP with type t = db ;;
module DbOrd : ORD with type t = db;;

val db_symb : db_symb bprint;;
val db_head : db_head bprint;;
val db : db bprint;;
val pdb : db bprint;;

val db_term : term -> db;;
val term_db : db -> term;;

val gen_terms : int -> typ -> terms;;
val gen_terms_hd : int -> ident -> terms;;
