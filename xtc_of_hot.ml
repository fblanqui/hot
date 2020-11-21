(**
HOT, a Higher-Order Termination prover
See the COPYRIGHTS and LICENSE files.

- Frederic Blanqui, 2010-09-15
*)

open Term;;
open Expr;;
open Xtc;;
open Util;;
open Lib;;

(*****************************************************************************)
(** maximal arity of a function symbol *)
(*****************************************************************************)

let rec arity_term m (h, ts) =
  List.fold_left arity_term (arity_head m (List.length ts) h) ts

and arity_head m k = function
  | Symb (Fun id) ->
      (try if k < StrMap.find id m then StrMap.add id k m else m
       with Not_found -> StrMap.add id k m)
  | Symb (Var _) -> m
  | Abs (_, t) -> arity_term m t;;
	  
let arity_rule m (l, r) = arity_term (arity_term m l) r;;

let arity_rules = List.fold_left arity_rule StrMap.empty;;

let arity id m = try StrMap.find id m with Not_found -> 0;;

(*****************************************************************************)
(** convert HOT terms into HO XTC terms *)
(*****************************************************************************)

let rec typ (typs, tid) =
  List.fold_right (fun t xt -> TypeArrow (typ t, xt)) typs (TypeBasic tid);;

let symb = function
  | Var x -> TermVar x
  | Fun f -> TermFunapp (f, []);;

let head term = function
  | Symb s -> symb s
  | Abs (l, t) ->
      List.fold_right (fun (x, ty) xt -> TermLam (x, typ ty, xt)) l (term t);;

let app term = List.fold_left (fun h ti -> TermApp (h, term ti));;

let term m =
  let rec aux (h, ts) =
    match h with
      | Symb (Fun f) ->
	  let n = arity f m and k = List.length ts in
	    if k < n then invalid_arg "Xtc_of_hot.aterm"
	    else let us, vs = Lib.split n ts in
	      app aux (TermFunapp (f, List.map aux us)) vs
      | Symb (Var _) | Abs _ -> app aux (head aux h) ts
  in aux;;

let rule m (l, r) = term m l, term m r, [];;

(*****************************************************************************)
(** convert a HOT problem into a HO XTC problem *)
(*****************************************************************************)

let typ_decl m f (typs, tid) =
  let us, vs = Lib.split (arity f m) typs in
    List.map typ us, typ (vs, tid);;    

let signature m sg = SignHO
  (StrMap.fold (fun s t l -> (s, typ t) :: l) sg.sig_vars [],
   StrMap.fold (fun s t l -> (s, typ_decl m s t) :: l) sg.sig_funs []);;

let trs m (sg, rs) = (List.map (rule m) rs, []), signature m sg, None, CTNone;;

let problem m p = Term, trs m p, StratFull, STNone, StatusNone, None;;

let cproblem = problem StrMap.empty;;

let aproblem ((_, rs) as p) = problem (arity_rules rs) p;;

(*****************************************************************************)
(** convert HOT terms into FO XTC terms *)
(*****************************************************************************)

let rec fo_term = function
  | Symb (Var x), [] -> TermVar x
  | Symb (Fun f), ts -> TermFunapp (f, List.map fo_term ts)
  | (Symb (Var _)|Abs _), _ ->
      invalid_arg "Xtc_of_hot.fo_term: non first-order term"

let fo_rule (l, r) = (fo_term l, fo_term r, []);;

(*****************************************************************************)
(** convert a HOT problem into a FO XTC problem *)
(*****************************************************************************)

let fo_signature sg = SignFO
  (StrMap.fold (fun s k l -> (s, k, ThyNone, None) :: l) sg []);;

let fo_trs (sg, rs) = (List.map fo_rule rs, []), fo_signature sg, None, CTNone;;

let fo_problem p = Term, fo_trs p, StratFull, STNone, StatusNone, None;;
