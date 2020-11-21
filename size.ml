(**
HOT, a Higher-Order Termination prover
See the COPYRIGHTS and LICENSE files.

- Frederic Blanqui, 2011-06-20
*)

open Util;;
open Acc;;
open Prec;;
open Lib;;
open Term;;
open Expr;;
open Printf;;
open Matrix;;

(*****************************************************************************)
(** sized types *)
(*****************************************************************************)

type size = Inf | Succ of int * ident;;

let svar id = Succ (0, id);;

let size b = function
  | Inf -> string b "oo"
  | Succ (k, id) -> if k = 0 then string b id else bprintf b "%s%+d" id k;;

type styp = styp list * ident * size;;

let arrow t (us, id, s) = t :: us, id, s;;
let arrows ts (us, id, s) = ts @ us, id, s;;

let rec styp b (ts, id, s) =
  bprintf b "%a%s[%a]" (list "" (postfix " => " pstyp)) ts id size s

and pstyp b ((ts, _, _) as t) = if ts = [] then styp b t else par styp b t;;

let size_of_styp (_, _, sz) = sz;;

(*****************************************************************************)
(** size constraints for a type to be a subtype of another type *)
(*****************************************************************************)

(** A triple (id1, id2, k) represents the linear constraint
    id1-id2<=k, i.e. id1<=s^k(id2) if k>=0, and s^(-k)(id1)<=id2
    otherwise. id1 and id2 must be distinct idents. Since
    {id1-id2<=k,id1-id2<=l} has the same solutions as
    {id1-id2<=min{k,l}}, we can represent sets of linear constraints
    by maps id1 -> id2 -> kmin where kmin is the smallest k such that
    there is a constraint id1-id2<=k. *)

type cons = int StrMap.t StrMap.t;;

let raw_add id1 id2 k cs =
  try let m = StrMap.find id1 cs in
    try
      if k < StrMap.find id2 m then StrMap.add id1 (StrMap.add id2 k m) cs
      else cs
    with Not_found -> StrMap.add id1 (StrMap.add id2 k m) cs
  with Not_found -> StrMap.add id1 (StrMap.add id2 k StrMap.empty) cs;;

let fold f = StrMap.fold (fun id1 m x -> StrMap.fold (f id1) m x);;
let iter f = StrMap.iter (fun id1 m -> StrMap.iter (f id1) m);;

let con b (id1, id2, k) =
  if k = 0 then bprintf b "%s <= %s" id1 id2
  else if k > 0 then bprintf b "%s <= %s+%d" id1 id2 k
  else bprintf b "%s+%d <= %s" id1 (-k) id2;;

let cons =
  let con b id1 id2 k = postfix ", " con b (id1, id2, k) in
    fun b -> iter (con b);;

(** a constraint problem is a pair (ids,cs) where ids is a set of
    variables equal to infinity and cs a set of linear constraints
    containing no variable of ids *)
type pb = StrSet.t * cons;;

let inf b = bprintf b "%s = oo";;
let infs b s = list ", " inf b (StrSet.elements s);;

let problem = pair infs ", " cons;;

(** add the constraint id=Inf to a constraint problem *)
let add_inf =
  let rec aux (newids, ((ids, cs) as x)) =
    try let id = StrSet.choose newids in
      aux
	(fold
	   (fun id1 id2 k ((newids, ((ids, cs) as x)) as y) ->
	      if id2 = id then y
	      else if id1 = id then StrSet.add id2 newids, x
	      else newids, (ids, raw_add id1 id2 k cs))
	   cs (StrSet.remove id newids, (StrSet.add id ids, StrMap.empty)))
    with Not_found -> x
  in fun id x -> aux (StrSet.singleton id, x);;

(** substitute in a size expression variables that are set to Inf in ids *)
let sub_inf ids = function
  | Inf -> Inf
  | Succ (_, id) as s -> if StrSet.mem id ids then Inf else s;;

(** add the constraints resulting of the comparison of size
    expressions s1 <= s2 *)
let add_cons ((ids, cs) as x) s1 s2 =
  match sub_inf ids s1, sub_inf ids s2 with
    | (Inf|Succ _), Inf -> x
    | Succ (k1, id1), Succ (k2, id2) ->
	if id1 = id2 then if k1 <= k2 then x else add_inf id1 x
	else ids, raw_add id1 id2 (k2-k1) cs
    | Inf, Succ (_k, id) -> add_inf id x;;

(** add the constraints resulting of the comparison of type
    expressions t1 <= t2 *)
let rec add_cons_styp x st1 st2 =
  (*debugf "add_cons_styp " (pair styp " <= " styp) (st1, st2);*)
  match st1, st2 with
    | ([], id1, s1), ([], id2, s2) ->
	if id1 = id2 then add_cons x s1 s2 else error "typing error"
    | (st1 :: sts1, id1, s1), (st2 :: sts2, id2, s2) ->
	add_cons_styp (add_cons_styp x st2 st1) (sts1, id1, s1) (sts2, id2, s2)
    | _, _ -> error "typing error";;

(*****************************************************************************)
(** functions building annotated types *)
(*****************************************************************************)

let a = svar "a";;

(** annotate all base types by Inf *)
let rec styp_inf (typs, tid) = List.map styp_inf typs, tid, Inf;;

(** annotate the output type ident by "a" and all idents in input
    types by Inf *)
let styp_output (typs, tid) = List.map styp_inf typs, tid, a;;

(** annotate all occurrences of tid0 by "a" and all other type ident by Inf *)
let styp_eq tid0 =
  let rec aux (typs, tid) =
    List.map aux typs, tid, if tid_eq tid tid0 then a else Inf
  in aux;;

(** annotate all positive occurrences of tid0 by "a" and all other
    ident occurrences by Inf *)
let styp_eq_pos tid0 =
  let rec pos (typs, tid) =
    List.map neg typs, tid, if tid_eq tid tid0 then a else Inf
  and neg (typs, tid) = List.map pos typs, tid, Inf
  in pos;;

(** annotate every type ident by a fresh variable *)
let rec styp_fresh (styps, tid) =
  List.map styp_fresh styps, tid, svar (fresh "a");;

(** annotate the output type ident of all input types by a fresh
    variable and all other idents by Inf *)
let styp_args (typs, tid) = List.map styp_fresh typs, tid, Inf;;

(** Given a type T1=>..=>Tn=>I, if some type equivalent to I occurs
    positively in at least one Ti, then annotate I by "a+1", every
    positive occurrence in T1..Tn of a type equivalent to I by "a",
    and all other type ident by Inf. Otherwise, annotate every type
    ident by a fresh variable. *)
let styp_cons ((typs, tid) as typ) =
  if List.exists (is_pos_in_typ tid) typs then
    List.map (styp_eq_pos tid) typs, tid, Succ (1, "a")
  else styp_fresh typ;;(*FIXME?*)

(** in a type T1=>..=>Tn=>I, annotate every type ident equivalent to I
    by "a" and all the other idents by Inf *)
let styp_max (typs, tid) = List.map (styp_eq tid) typs, tid, a;;

(*****************************************************************************)
(** size substitutions *)
(*****************************************************************************)

(** apply a size substitution on a size expression *)
let sub_size s = function
  | Inf -> Inf
  | Succ (k, id) as sz ->
      try match StrMap.find id s with
	| Inf -> Inf
	| Succ (k', id') -> Succ (k+k', id')
      with Not_found -> sz;;

(** apply a size substitution on a sized type *)
let sub_styp s =
  let rec aux (styps, tid, sz) = List.map aux styps, tid, sub_size s sz
  in aux;;

(** size variables occurring in a sized type *)
let vars_styp =
  let rec aux vs (styps, _, sz) =
    List.fold_left aux
      (match sz with Inf -> vs | Succ (_, id) -> StrSet.add id vs) styps
  in aux StrSet.empty;;

(** substitute each variable occurring in a sized type by a fresh variable *)
let sub_styp_fresh st =
  sub_styp
    (StrSet.fold (fun id s -> StrMap.add id (svar (fresh "a")) s)
       (vars_styp st) StrMap.empty)
    st;;

(*****************************************************************************)
(** size constraints for a term to be well-typed *)
(*****************************************************************************)

let styp_ident m id =
  try StrMap.find id m
  with Not_found ->
    invalid_arg ("Size.styp_ident: no type provided for " ^ id);;

let styp_ident_fresh m id = sub_styp_fresh (styp_ident m id);;

let styp_symb fmap vm fm = function
  | Fun id ->
      let st = styp_ident_fresh fmap id in
	debugf (id ^ " : ") styp st;
	st, if is_defined id then (id, st) :: fm else fm
  | Var id -> styp_ident vm id, fm;;

let rec styp_term fmap vm fm x (h, ts) =
  let (typs, tid, s), fm, x = styp_head fmap vm fm x h in
    styp_app fmap vm fm x typs tid s ts

and styp_head fmap vm fm x = function
  | Symb s -> let st, fm = styp_symb fmap vm fm s in st, fm, x
  | Abs (idtyps, t) ->
      let styps, vm =
	List.fold_left
	  (fun (styps, vm) (id, ty) ->
	     let styp = styp_fresh ty in
	       styp :: styps, StrMap.add id styp vm)
	  ([], vm) idtyps in
      let styp, fm, x = styp_term fmap vm fm x t in
	arrows (List.rev styps) styp, fm, x

and styp_app fmap vm fm x styps tid s ts =
  match styps, ts with
    | _, [] -> (styps, tid, s), fm, x
    | styp :: styps', t :: ts' ->
	let styp', fm, x = styp_term fmap vm fm x t in
	  styp_app fmap vm fm (add_cons_styp x styp' styp) styps' tid s ts'
    | _, _ -> error "typing error";;

(*****************************************************************************)
(** variables that must be set to infinity using Floyd-Warshall algorithm *)
(*****************************************************************************)

(** operations on (Some) integers extended with +infinity (None) *)
let add x y =
  match x, y with
    | None, _ | _, None -> None
    | Some x, Some y -> Some (x+y);;

let lt x y =
  match x, y with
    | Some x, Some y -> x < y
    | Some _, None -> true
    | None, _ -> false;;

let cost b = function
  | Some k -> int b k
  | None -> string b "oo";;

(** Floyd-Warshall algorithm returning a predecessor matrix also for
    reconstructing paths *)
let floyd_warshall path =
  let n = Matrix.nb_lines path in
  let pred = Matrix.init n n (fun i _ -> i) in
    (*debugf "pred:\n" (matrix int) pred;*)
    for k = 0 to n-1 do
      for i = 0 to n-1 do
	for j = 0 to n-1 do
	  let c = add (get path i k) (get path k j) in
	    if lt c (get path i j) then
	      (set path i j c; set pred i j (get pred k j) (*;
	      debugf "i, j, k = " (list ", " int) [i; j; k];
	      debugf "path: " (matrix cost) path;
              debugf "pred:\n" (matrix int) pred*))
	done
      done
    done;
    path, pred;;

(** function for reconstructing paths *)
let get_path pred i =
  let rec aux acc j = if i=j then j::acc else aux (j::acc) (get pred i j) in
    fun j -> List.rev (aux [] j);;

(** compute negative cycles *)
let negative_cycles path pred =
  let rec aux i cys =
    if i < 0 then cys else aux (i-1)
      (match get path i i with
	 | Some k -> if k < 0 then get_path pred i i :: cys else cys
	 | None -> cys)
  in aux (Matrix.nb_lines path - 1) [];;

(** give a number to each node *)
let node_maps cs =
  let n = ref (-1) in
  let add x ((m1, m2) as m) =
    if StrMap.mem x m1 then m
    else (incr n; StrMap.add x !n m1, IntMap.add !n x m2)
  in fold (fun x y _ m -> add x (add y m)) cs (StrMap.empty, IntMap.empty);;

(** build an initial path matrix from a set of constraints *)
let edge_cost m1 cs =
  let n = StrMap.cardinal m1 in
  let path = Matrix.make n n None in
    iter
      (fun id1 id2 k ->
	 set path (StrMap.find id1 m1) (StrMap.find id2 m1) (Some k))
      cs;
    for i = 0 to n-1 do set path i i (Some 0) done;
    path;;

(** variables that must be set to infinity *)
let inf_vars cs =
  let m1, m2 = node_maps cs in
    (*debugf "m1: " (map "=" int ", ") m1;*)
  let path = edge_cost m1 cs in
    (*debugf "path:" (matrix cost) path;*)
  let path, pred = floyd_warshall path in
  let cys = negative_cycles path pred in
    (*debugf "cycles: " (list "; " (list "," int)) cys;*)
    List.fold_left
      (fun ids cy ->
	 List.fold_left
	   (fun ids i -> StrSet.add (IntMap.find i m2) ids)
	   ids cy)
      StrSet.empty cys;;

let detect_inf_vars ((_, cs) as x) = StrSet.fold add_inf (inf_vars cs) x;;

(*****************************************************************************)
(** solve constraints by calling an external tool *)
(*****************************************************************************)

let ignore_lines ic =
  let rec aux k =
    if k > 0 then (let _ = input_line ic in aux (k-1)) in aux;;

let input_sub ic =
  let rec aux x =
    try let s, d = Scanf.fscanf ic "%s %d\n" (fun s d -> s,d) in
      aux (StrMap.add s d x)
    with End_of_file -> x
  in aux StrMap.empty;;

let get_lps_result outfn =
  let ic = try open_in outfn with Sys_error s -> error s in
    ignore_lines ic 4;
    let s = input_sub ic in
      close_in ic;
      s;;

let cons_vars cs =
  fold (fun id1 id2 _k vs -> StrSet.add id1 (StrSet.add id2 vs))
    cs StrSet.empty;;

let output_lp oc cs =
  let vs = cons_vars cs in
    fprintf oc "min: %a;\n\n%a\nint %a;\n" (fprint (Util.set " + ")) vs
      (fun oc -> iter (fprintf oc "%s <= %s%+d;\n")) cs
      (fprint (Util.set ", ")) vs;;

(** variables that must be send to the same variable *)
let vmap cs = fold
  (fun id1 id2 _k m ->
     try let repid1 = StrMap.find id1 m in
       (* id1 is mapped to repid1 *)
       try let repid2 = StrMap.find id2 m in
	 (* id2 is mapped to repid2 *)
	 if repid1 = repid2 then m
	 else StrMap.map (fun id -> if id = repid2 then repid1 else id) m
       with Not_found -> (* id2 is not mapped *)
	 StrMap.add id2 repid1 m
     with Not_found -> (* id1 is not mapped *)
       try let repid2 = StrMap.find id2 m in
	 (* id2 is mapped to repid2 *)
	 StrMap.add id1 repid2 m
       with Not_found -> (* id2 is not mapped *)
	 StrMap.add id1 id1 (StrMap.add id2 id1 m))
  cs StrMap.empty;;

let call_lp_solver cs =
  if cs = StrMap.empty then Some (StrMap.empty)
  else if is_set_lp_solver() then begin
    let lpfn = new_temp "hot" ".lp" in
      output_file output_lp lpfn cs;
      let outfn = new_temp_change_postfix lpfn ".out" in
      let cmd = Printf.sprintf "%s %s > %s 2>&1" (get_lp_solver()) lpfn outfn in
	if command cmd <> 0 then
	  (verbose "no minimal solution found"; None)
	else
	  let m = vmap cs and s = get_lps_result outfn in
	  let var id = try StrMap.find id m with Not_found -> id in
	    Some (StrMap.fold
		    (fun id k m ->
		       let x = var id in
			 if k=0 && x = id then m
			 else StrMap.add id (Succ (k, x)) m)
		    s StrMap.empty)
  end else None;;

let sol (ids, cs) =
  verbose "try to solve size constraints...";
  match call_lp_solver cs with
    | None -> None
    | Some s -> Some (StrSet.fold (fun id -> StrMap.add id Inf) ids s);;

(*****************************************************************************)
(** conditions used in size-based termination *)
(*****************************************************************************)

(** check weak linearity wrt size variables *)
let is_weak_lin styps us =
  try
    ignore
      (Lib.fold_left2
	 (fun m (styps, tid, sz) u ->
	    if not (styps = [] && is_pos tid && not (is_basic tid)) then m
	    else match sz with
	      | Inf -> m
	      | Succ (_, id) ->
		  try if StrMap.find id m = u then m else raise Exit 
		  with Not_found -> StrMap.add id u m)
	 StrMap.empty styps us);
    true
  with Exit -> verbose "size annotations are not weakly linear"; false;;

(** the inductive positions of a pattern t is the list of pairs (id,d)
    where id is the name of a free variable occurring in t at depth d *)
let ind_pos =
  let rec aux d l = function
    | Symb (Var id), _ -> (id,d)::l
    | Symb (Fun cid), ts -> List.fold_left (aux (d+1)) l (rec_args cid ts)
    | Abs (_, t), _ -> aux d l t
  in aux 0 [];;

(** check minimality and pattern condition *)
let is_pat_cond styps us =
  Lib.for_all2
    (fun (_, tid, sz) u ->
       is_basic tid
       || match sz with
	 | Inf -> true
	 | Succ (k, _) ->
	     let ips = ind_pos u in
	     let max =
	       List.fold_left (fun m (_,d) -> Pervasives.max m d) 0 ips
	     in (*debugf "k, max = " (pair int ", " int) (k, max);
	       debugf "ips = " (list ", " (pair string "/" int)) ips;*)
	       k = max && List.for_all (fun (_, d) -> d >= max - 1) ips)
    styps us
  || (verbose "pattern condition not satisfied"; false);;

let is_sbt fid styps us =
  not (is_rec fid) || (is_pat_cond styps us && is_weak_lin styps us);;

let sat_sbt_conds =
  List.for_all (fun (fid, us, (styps, _, _), _) -> is_sbt fid styps us);;

(*****************************************************************************)
(** type-checking rewrite rules *)
(*****************************************************************************)

(** gives an annotated typ to the free variables of some terms *)
let var_styp_map =
  List.fold_left
    (fold_vars (fun x _ m -> StrMap.add x (styp_fresh (typ_of_ident x)) m))
    StrMap.empty;;

(** constraints for typing a rule *)
let cons_styp_rule fmap fid us r =
  debugf "\nrule " rule (appfunc fid us, r);
  let vm = var_styp_map us in
    debugf "" (map " : " styp "\n") vm;
  let x = StrSet.empty, StrMap.empty in
  let (styps, tid, s) as st = styp_ident_fresh fmap fid in
  let styp1, _, x = styp_app fmap vm [] x styps tid s us in
  let styp2, fm, x = styp_term fmap vm [] x r in
  let x = add_cons_styp x styp2 styp1 in
  let fm = List.filter (fun (id, _) -> def_cmp id fid = Eq) fm in
  let p = detect_inf_vars x in
    debugf "constraints: " problem p;
    st, fm, p;;

let add_cons_styp_rule fmap ((dps, ids, cs) as x) (fid, us, r) =
  if List.mem r us then x (*IMPROVE*)
  else
    let st, fm, (ids', cs') = cons_styp_rule fmap fid us r in
      (fid, us, st, fm) :: dps, StrSet.union ids' ids, adds cs' cs;;

let cons_styp_rules fmap =
  List.fold_left (add_cons_styp_rule fmap) ([], StrSet.empty, StrMap.empty);;

(*****************************************************************************)
(** safe terms *)
(*****************************************************************************)

(** superset of head symbols of the redexes of a term. raise Not_found
    if some redex is headed by a variable or an abstraction *)
let heads =
  let rec aux fs (h, _) =
    match h with
      | Symb (Fun f) ->
	  if StrSet.mem f fs then fs
	  else List.fold_left (fun fs (_, r) -> aux fs r)
	    (StrSet.add f fs) (rules_of_ident f)
      | Symb (Var _) | Abs _ -> raise Not_found
  in aux StrSet.empty;;

let heads = debug1' "heads " term (Util.set ",") heads;;

(** symbols having a rule matching on some symbol of [fs] *)
let funs_matching ids =
  StrMap.fold
    (fun f rs fs ->
       if List.exists
	 (fun ((_, ls), _) ->
	    List.exists
	      (exists_fun (fun g _ -> StrSet.mem g ids))
	      ls)
	 rs
       then StrSet.add f fs else fs)
    (get_rules_map())
    StrSet.empty;;

let funs_matching =
  debug1' "funs_matching " (pset ",") (pset ",") funs_matching;;

let is_safe t =
  try StrSet.for_all is_comp (funs_matching (heads t))
  with Not_found -> false;;

(*****************************************************************************)
(** computability closure *)
(*****************************************************************************)

let is_comp_neg_var_app x rs =
  let typs, tid = typ_of_ident x in
    List.length typs = List.length rs
      && is_basic tid
      && List.for_all is_basic_typ typs
      && rs <> []
      && List.for_all is_safe rs;;

let is_in_cc ls =
  let is_acc_sub vm t = List.exists (is_acc_subterm_eq vm t) ls in

  let rec aux vm ((h, rs) as r) =
    (*debugf "is_in_cc_aux: " (pair (pmap "=" typ ",") " " pterm) (vm, r);*) 
    is_bound_var vm r
    || match aux_app_split vm h rs [] with
      | Some us -> List.for_all (aux vm) us
      | None ->
	  (match h with
	     | Symb (Var x) -> StrMap.mem x vm || is_comp_neg_var_app x rs
	     | Symb (Fun _) -> true
	     | Abs (idtyps, t) -> aux (add_var_typs vm idtyps) t)
	  && List.for_all (aux vm) rs

  and aux_app_split vm h rs1 rs2 =
    if is_acc_sub vm (h, rs1) then Some rs2
    else match rs1 with
      | [] -> None
      | t1 :: ts' ->
	  let rs1', tn = split_last t1 ts' in
	    aux_app_split vm h rs1' (tn :: rs2)

  in aux StrMap.empty;;

(*let is_in_cc = debug2' "is_in_cc " (ml_list term) pterm bool is_in_cc;;*)

let check_cc_rule rs ((h, ts), r) =
  match h with
    | Symb (Var _) | Abs _ -> error "lhs not headed by a function symbol"
    | Symb (Fun f) ->
	if is_in_cc ts r then (f, ts, r) :: rs
	else error "rhs not in the computability closure of the lhs";;

let check_cc_fun_class =
  List.fold_left
    (fun rs f -> List.fold_left check_cc_rule rs (rules_of_ident f))
    [];;

let complete f ts = ts @ fresh_vars "x" (arity_fun f - List.length ts);;

let dps_gs =
  List.fold_left
    (fun dps (f, ts, r) ->
       fold_funs
	 (fun g us dps ->
	    if is_defined g && def_cmp f g = Eq
	    then (f, complete f ts, g, complete g us) :: dps
	    else dps)
	 dps r)
    [];;

(*****************************************************************************)
(** size-based termination *)
(*****************************************************************************)

let id_inf = ref "oo" and id_succ = ref "S";;
let inf = ref (func !id_inf) and hsucc = ref (Symb (Fun !id_succ));;

let init_inf_and_S() =
  id_inf := fresh "oo";
  id_succ := fresh "S";
  inf := func !id_inf; hsucc := Symb (Fun !id_succ);;

let succ k t =
  let rec aux k = if k > 0 then !hsucc, [aux (k-1)] else t
  in aux k;;

let term_of_size = function
  | Inf -> !inf
  | Succ (k, id) -> succ k (var id);;

let term_of_styp st = term_of_size (size_of_styp st);;

let dps_sbt =
  List.fold_left
    (fun dps (f, _, (styps,_,_), fm) ->
       let szs = List.map term_of_styp styps in
	 List.fold_left
	   (fun dps (g, (styps,_,_)) ->
	      (f, szs, g, List.map term_of_styp styps) :: dps)
	   dps fm)
    [];;

(*****************************************************************************)
(** call matrices *)
(*****************************************************************************)

open Call;;

let rel_term r l =
  if alpha_eq_bv StrMap.empty r l then Eq
  else if is_subterm_eq r l then Lt
  else Un;;

let rel_term ((h, rs) as r) l =
  match rel_term r l with
    | (Eq | Lt) as c -> c
    | Un ->
	if rs = [] then Un else
	  match h with
	    | Symb (Var _) -> rel_term (h, []) l
	    | Symb (Fun _) | Abs _ -> Un;;

let add_call_matrix_of_dp rel cs (f, ts, g, us) =
  let a =
    let t = Array.of_list ts and u = Array.of_list us in
      Matrix.init (Array.length u) (Array.length t)
	(fun i j -> rel u.(i) t.(j))
  in CMSet.add (f, g, a) cs;;

let call_matrices_of_dps rel =
  List.fold_left (add_call_matrix_of_dp rel) CMSet.empty;;

let dp b (f, ts, g, us) = rule b (appfunc f ts, appfunc g us);;

let check_termin rs =
  verbosef "\ndependency pairs:\n" (list "\n" dp) rs;
  let cs = Call.completion (call_matrices_of_dps rel_term rs) in
    debugf "\ncall matrices:\n" call_matrices cs;
  let m = Call.lex_orders cs in
    verbosef "\nlexical orders:\n" (map ": " (list "," int) "\n") m;
  if StrMap.exists (fun _ l -> l = []) m then error "failed";;

(*****************************************************************************)
(** heuristic to find a valid annotated type for every function symbol *)
(*****************************************************************************)

let teq tid (typs', tid') = typs' = [] && tid_eq tid' tid;;

let steq tid (styps', tid', _) = styps' = [] && tid_eq tid' tid;;

let add_styps_nsi ty l =
  let styps, tid, _ = styp_args ty in
    List.fold_left
      (fun l ((_,_,sz) as st) ->
	 if steq tid st then (styps, tid, sz) :: l else l)
      l styps;;

let is_nsi_eq tid (typs', tid') =
  tid_eq tid' tid && List.exists (teq tid) typs';;

let add_styp_max ((typs, tid) as ty) l =
  if exists2 (teq tid) typs
    || (List.exists (teq tid) typs && List.exists (is_nsi_eq tid) typs)
  then styp_max ty :: l
  else l;;

let possible_styps id =
  let ty = typ_of_ident id in
    if is_defined id then
      if is_cons id then [styp_args ty]
      else add_styp_max ty (add_styps_nsi ty [styp_args ty])
    else
      if is_cons id then [styp_cons ty]
      else [styp_args ty];;

let funs_rule (f, ts, r) = funs_rule (appfunc f ts, r);;

let funs_rules =
  List.fold_left (fun s r -> StrSet.union s (funs_rule r)) StrSet.empty;;

let find_styp rs =
  let fs = StrSet.elements (funs_rules rs) in
  let f = Array.of_list fs
  and fmap c = fold_left_i
    (fun m i fi -> StrMap.add fi c.(i) m) StrMap.empty fs in
  let l = Array.init (Array.length f)
    (fun i -> Array.of_list (possible_styps f.(i))) in
    debug "\npossible types:";
    Array.iteri (fun i li ->
		   debugf (f.(i) ^ ":") (array "" (prefix "\n" styp)) li) l;
  let next = next l in
  let check() =
    let fmap = fmap (next()) in
      small_sep();
      verbosef "try with the following types:\n" (map " : " styp "\n") fmap;
    let dps, ids, cs = cons_styp_rules fmap rs in
      match sol (ids, cs) with
	| None -> raise Error
	| Some s ->
	    let dps =
	      List.map (fun (f, us, st, fm) -> f, us, sub_styp s st,
			  List.map (fun (f, st) -> f, sub_styp s st) fm)
		dps in
	      if not (sat_sbt_conds dps) then raise Error
	      else check_termin (dps_sbt dps) in
  let rec aux() =
    try check() with
      | Error -> aux()
      | Not_found -> error "no valid size annotation found"
  in aux();;

(*****************************************************************************)
(** main *)
(*****************************************************************************)

let prove_fun_class fs =
  let rs = check_cc_fun_class fs in
    begin try
      big_sep(); verbosef ("try size-based termination for ")
	(postfix "..." (list "," string)) fs;
      find_styp rs
    with Error ->
      big_sep(); verbosef ("try general schema for ")
	(postfix "..." (list "," string)) fs;
      check_termin (dps_gs rs)
    end;
    add_comp_funs (set_of_list fs);;

let order_funs() =
  let fs = get_funs() in
  let l = StrPrec.linear
    (StrPrec.filter (fun f -> StrSet.mem f fs) (get_def_prec())) in
    verbosef "linear ordering of defined symbols: "
      (list "; " (list "," string)) l;
    l;;

let try_sbt() =
  big_sep(); verbose "try size-based termination or general schema...";
  let l = order_funs() in
    build_typ_prec l;
    init_inf_and_S();
    List.iter prove_fun_class l;
    quit Yes;;
