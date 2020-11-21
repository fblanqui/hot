(**
HOT, a Higher-Order Termination prover
See the COPYRIGHTS and LICENSE files.

- Frederic Blanqui, 2010-09-15

convert XTC format into HOT format
*)

open Term;;
open Expr;;
open Xtc;;
open Util;;
open Lib;;

(** print an XTC term. for debug *)

(*DEBUG: open Printf;;

let rec xtc b = function
  | TermVar v -> bprintf b "(var %s)" v
  | TermLam (v, _ty, tm) -> bprintf b "(lam %s. %a)" v xtc tm
  | TermApp (t, u) -> bprintf b "(app %a %a)" xtc t xtc u
  | TermFunapp (f, ts) ->
      bprintf b "(funapp %s%a)" f (list "" (prefix " " xtc)) ts;;

let rec xtyp b = function
  | TypeBasic s -> string b s
  | TypeArrow (t1, t2) -> bprintf b "%a => %a" pxtyp t1 pxtyp t2

and pxtyp b t =
  match t with
    | TypeBasic _ -> xtyp b t
    | TypeArrow _ -> par xtyp b t;;*)

(** convert a string into a valid HOT identifier *)

let ident_of_string = StrMem.memo
  (fun s ->
     let t =
       try match Lexer.token (Lexing.from_string s) with
	 | Parser.IDENT t when t = s -> s
	 | _ -> raise Parsing.Parse_error
       with Parsing.Parse_error ->
	 let t = fresh "x" in
	   verbose (Printf.sprintf "\"%s\" renamed into \"%s\"" s t); t
     in notin (StrSet.singleton t); t);;

(** types *)

let rec typ = function
  | TypeBasic s -> typ_const (ident_of_string s)
  | TypeArrow (t1, t2) -> arrow (typ t1) (typ t2);;

(** set of type constants in an XTC declaration *)

let rec typ_consts_typ set = function
  | TypeBasic s -> StrSet.add (ident_of_string s) set
  | TypeArrow (t1, t2) -> typ_consts_typ (typ_consts_typ set t1) t2;;

let typ_consts_typs = List.fold_left typ_consts_typ;;

let typ_consts vs fs =
  List.fold_left
    (fun set (_, (ts, t)) -> typ_consts_typ (typ_consts_typs set ts) t)
    (List.fold_left
       (fun set (_, t) -> typ_consts_typ set t)
       StrSet.empty
       vs)
    fs;;

(** terms are converted into raw lambda expressions in Curry form *)

let rec expr = function
  | TermVar v -> EIdent (ident_of_string v)
  | TermLam (v, ty, tm) -> EAbs (ident_of_string v, typ ty, expr tm)
  | TermApp (t, u) -> EApp (expr t, expr u)
  | TermFunapp (f, ts) ->
      List.fold_left (fun e t -> EApp (e, expr t))
	(EIdent (ident_of_string f)) ts;;

(** rules *)

let expr_rule = function
  | l, r, [] -> expr l, expr r
  | _, _, _ -> error "conditional rules are not supported";;

(** function type declarations *)

let fun_typ (ts, t) = arrows (List.map typ ts) (typ t);;

(* declared arity <= (maximal) arity given by type *)
let arity_map = ref StrMap.empty;;

let get_arity id =
  try StrMap.find id !arity_map
  with Not_found -> invalid_arg "Hot_of_xtc.get_arity";;

let sig_funs =
  List.fold_left
    (fun m (s, ((ts, _) as td)) ->
       let id = ident_of_string s in
	 arity_map := StrMap.add id (List.length ts) !arity_map;
	 StrMap.add id (fun_typ td) m)
    StrMap.empty;;

(** variable type declarations *)

let sig_vars =
  List.fold_left
    (fun m (s, t) -> StrMap.add (ident_of_string s) (typ t) m)
    StrMap.empty;;

(** problems *)

let trs = function
  | (rs, _), SignHO (vs, fs), _, CTNone ->
      { sig_typs = typ_consts vs fs; sig_funs = sig_funs fs;
	sig_vars = sig_vars vs }, List.map expr_rule rs
  | _, _, _, _ -> error "class of problem not supported";;

let expr_problem_of_xtc_problem p =
  warning "we put terms in Curry form";
  match p with
    | Term, s, StratFull, STNone, _, _ -> trs s
    | _, _, _, _, _, _ -> error "class of problem not supported";;
