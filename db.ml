(**
HOT, a Higher-Order Termination prover
See the COPYRIGHTS and LICENSE files.

- Frederic Blanqui, 2011-10-28
*)

open Printf;;
open Util;;
open Lib;;
open Term;;
open Expr;;

(*****************************************************************************)
(** de Bruijn terms *)
(*****************************************************************************)

(*IMPROVE: use hashconsing *)

type db = db_head * db list
and db_head = Db_symb of db_symb | Db_abs of typ list * db
and db_symb = Db_fun of ident | Db_var of int (* >= 0 *);;

let db_symb b = function
  | Db_fun id -> string b id
  | Db_var k -> int b k;;

let rec raw_db b t = pair raw_db_head ", " (ml_list raw_db) b t
and raw_db_head b = function
  | Db_symb sy -> bprintf b "Db_symb %a" db_symb sy
  | Db_abs (tys, t) -> bprintf b "Db_abs(%a,%a)" (ml_list typ) tys praw_db t
and praw_db b ((h, ts) as t) =
  if ts = [] then raw_db_head b h else par raw_db b t;;

let rec db b t = pair db_head "" (list "" (prefix " " pdb)) b t
and db_head b = function
  | Db_symb sy -> db_symb b sy
  | Db_abs ([], t) -> db b t
  | Db_abs (ty :: tys, t) ->
      bprintf b "(%a.%a)" typ ty db_head (Db_abs (tys, t))
and pdb b ((h, ts) as t) = if ts = [] then db_head b h else par db b t;;

let db_term =
  let rec aux ids (h, ts) = head ids h, List.map (aux ids) ts
  and head ids = function
    | Symb sy -> Db_symb (symb ids sy)
    | Abs (idtyps, t) ->
	let ids', tys = List.split idtyps in
	  Db_abs (tys, aux (List.rev_append ids' ids) t)
  and symb ids = function
    | Fun f -> Db_fun f
    | Var x -> Db_var (position x ids)
  in aux [];;

let term_db =
  let rec aux ids (h, ts) = head ids h, List.map (aux ids) ts
  and head ids = function
    | Db_symb sy -> Symb (symb ids sy)
    | Db_abs (tys, t) ->
	let ids' = List.map (fun _ -> fresh "x") tys in
	  Abs (List.combine ids' tys, aux (List.rev_append ids' ids) t)
  and symb ids = function
    | Db_fun f -> Fun f
    | Db_var k -> Var (List.nth ids k)
  in aux [];;

let rec size (h, ts) = List.fold_left (fun s t -> s + size t) (size_head h) ts
and size_head = function
  | Db_symb _ -> 1
  | Db_abs (_, t) -> 1 + size t;;

(*****************************************************************************)
(** sets of db terms *)
(*****************************************************************************)

module Db = struct type t = db end;;
module DbOrd = Ord.Make (Db);;

let max_size = 300;;

module DbSet : sig
  type t
  val empty : t
  val singleton : db -> t
  val add : db -> t -> t
  val union : t -> t -> t
  val elements : t -> db list
  val fold : (db -> 'a -> 'a) -> t -> 'a -> 'a
end
= struct
  module DbSet = Set.Make (DbOrd)
  type t = { set : DbSet.t; size : int; too_big : bool }
  let too_big s =
    if s.too_big then s else (debug "set too big"; {s with too_big = true})
  let empty = { set = DbSet.empty; size = 0; too_big = false }
  let singleton x = { set = DbSet.singleton x; size = size x; too_big = false }
  let union s t =
    let n = s.size + t.size in
      if n < max_size then
	{ set = DbSet.union s.set t.set; size = n; too_big = false }
      else too_big t
  let add x s = union (singleton x) s
  let elements s = DbSet.elements s.set
  let fold f s = DbSet.fold f s.set
  let dbset b s = bprintf b "{%a}" (list ", " db) (DbSet.elements s.set)
end;;

(*****************************************************************************)
(** generate db terms of a given type *)
(*****************************************************************************)

let add_global_fresh ty =
  let x = fresh "x" in add_global_symb (Var x) ty; Db_symb (Db_fun x), [];;

let add_global_freshs_typ ty =
  let rec aux acc n =
    if n <= 0 then acc
    else aux (DbSet.add (add_global_fresh ty) acc) (n-1)
  in aux DbSet.empty;;

let nb_vars = 2;; (** maximum number of free variables *)

let dbs0 = StrMem.memo
  (fun tid -> DbSet.singleton (add_global_fresh ([], tid)));;
  (*REMOVE:add_global_freshs_typ ([], tid) nb_vars);;*)

module T = struct type t = typ list * int * string end;;
module TOrd = Ord.Make (T);;
module TMap = Map.Make (TOrd);;

let rec dbs_aux0 =
  let m = ref TMap.empty in
    fun tys0 k tid ->
      try TMap.find (tys0, k, tid) !m
      with Not_found -> (*let res =*)
  if k <= 0 then
    let l, _ = List.fold_left
      (fun (l, k) (tys, tid') ->
	 (if tys = [] && tid' = tid then DbSet.add (Db_symb (Db_var k), []) l
	  else l), k+1)
      (dbs0 tid, 0) tys0
    in l
  else
    List.fold_left
      (fun l ftys -> DbSet.union (dbs_aux1 tys0 (k-1) ftys) l)
      (dbs_aux0 tys0 (k-1) tid) (lfind tid (get_typ_map()))
(*in debugf "dbs_aux0 " (pair (ml_list typ) " " (pair int " " (pair string " " (dbset db)))) (tys0,(k,(tid,res))); res*)

and dbs_aux1 tys0 k (f, tys) = (*let res =*)
  List.fold_left
    (fun l ts -> DbSet.add (Db_symb (Db_fun f), ts) l)
    DbSet.empty
    (enum (List.fold_left
	     (fun ll ty -> DbSet.elements (dbs_aux2 tys0 k ty) :: ll)
	     [] (List.rev tys)))
(*    in debugf "dbs_aux1 "
	 (pair (ml_list typ) " " (pair int " " (pair string " " (pair (ml_list typ) " = " (dbset db)))))
    (tys0,(k,(f,(tys,res)))); res*)

and dbs_aux2 tys0 k (tys, tid) = (*let res =*)
  let s = dbs_aux0 (List.rev_append tys tys0) k tid in
    if tys = [] then s
    else
      DbSet.fold
	(fun t l -> DbSet.add (Db_abs (tys, t), []) l)
	s DbSet.empty
(*    in debugf "dbs_aux2 "
    (pair (ml_list typ) " " (pair int " " (pair (ml_list typ) " " (pair string " = " (dbset db)))))
    (tys0,(k,(tys,(tid,res)))); res;;*)

let gen_dbs k ty = dbs_aux2 [] k ty;;

(*let gen_dbs = debug2 "gen_dbs " int typ (dbset db) gen_dbs;;*)

let gen_dbs_hd k f =
  let tys, _ = typ_of_ident f in dbs_aux1 [] (k-1) (f, tys);;

(*let gen_dbs_hd = debug2 "gen_dbs_hd " int string (dbset db) gen_dbs_hd;;*)

let terms_of_dbs s = DbSet.fold (fun d l -> term_db d :: l) s [];;

let gen_terms k ty = terms_of_dbs (gen_dbs k ty);;

let gen_terms_hd k f = terms_of_dbs (gen_dbs_hd k f);;
