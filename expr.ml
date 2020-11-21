(**
HOT, a Higher-Order Termination prover
See the COPYRIGHTS and LICENSE files.

- Frederic Blanqui, 2010-03-15
*)

open Term;;
open Util;;
open Printf;;
open Lib;;
open Prec;;

(*****************************************************************************)
(** types *)
(*****************************************************************************)

let get_typ_consts, set_typ_consts = get_set StrSet.empty;;

let check_typ_const id =
  if not (StrSet.mem id (get_typ_consts())) then error (id ^ " not declared");;

let rec check_typ (ts, id) = check_typ_const id; List.iter check_typ ts;;

(*****************************************************************************)
(** raw lambda expressions provided by parsing: it is not known
    whether an identifier is a variable or a function symbol yet *)
(*****************************************************************************)

type expr =
  | EIdent of ident
  | EAbs of ident * typ * expr
  | EApp of expr * expr;;

let rec expr_app e1 = function
  | [] -> e1
  | e2 :: es -> expr_app (EApp (e1, e2)) es;;

let rec raw_expr b = function
  | EIdent id -> bprintf b "%s" id
  | EAbs (id, t, e) -> bprintf b "(%s:%a.%a)" id typ t raw_expr e
  | EApp (e1, e2) -> bprintf b "(%a %a)" raw_expr e1 raw_expr e2;;

let rec expr b = function
  | EIdent id -> bprintf b "%s" id
  | EAbs (id, t, e) -> bprintf b "(%s:%a.%a)" id typ t expr e
  | EApp (e1, e2) -> bprintf b "%a %a" expr e1 pexpr e2

and pexpr b = function
  | EIdent id -> bprintf b "%s" id
  | EAbs (id, t, e) -> bprintf b "(%s:%a.%a)" id typ t expr e
  | EApp (e1, e2) -> bprintf b "(%a %a)" expr e1 pexpr e2

type expr_rule = expr * expr;;
type expr_rules = expr_rule list;;

let expr_rule = rule_gen expr;;

(*****************************************************************************)
(** map identifier -> symbol * typ *)
(*****************************************************************************)

let get_symb_typ_map, set_symb_typ_map = get_set StrMap.empty;;

let add_global_symb s typ =
  let id = ident_of_symb s in 
    if StrMap.mem id (get_symb_typ_map()) then
      error (id ^ " already declared")
    else begin
      check_typ typ;
      set_symb_typ_map (StrMap.add id (s, typ) (get_symb_typ_map()))
    end;;

let add_global_fresh ty =
  let v = Var (fresh "x") in add_global_symb v ty; symb v;;

let add_global_freshs_typ ty =
  let rec aux n = if n <= 0 then [] else add_global_fresh ty :: aux (n-1)
  in aux;;

let symb_of_ident id =
  try fst (StrMap.find id (get_symb_typ_map()))
  with Not_found -> error (id ^ " not declared");;

let typ_of_ident id =
  try snd (StrMap.find id (get_symb_typ_map()))
  with Not_found -> error (id ^ " not declared");;

let typs_of_idents s =
  StrSet.fold
    (fun id m -> StrMap.add id (typ_of_ident id) m) s StrMap.empty;;

let arity_fun id = arity_typ (typ_of_ident id);;

(*****************************************************************************)
(** convert a raw expression into a well-formed term *)
(*****************************************************************************)

let term_of_expr =
  let rec aux vmap = function
    | EIdent id ->
	symb (try StrMap.find id vmap with Not_found -> symb_of_ident id)
    | EAbs (x, typ, e) -> abs1 x typ (aux (StrMap.add x (Var x) vmap) e)
    | EApp (e1, e2) -> app (aux vmap e1) (aux vmap e2)
  in aux StrMap.empty;;

(*****************************************************************************)
(** convert rules so that bound variables are pairwise distinct and
    distinct from free variables *)
(*****************************************************************************)

let rule_of_expr_rule (e1, e2) =
  let l = term_of_expr e1 and r = term_of_expr e2 in
    notin (vars l);
    let l', r' = rename_bound_vars l, rename_bound_vars r in
      if (l, r) <> (l',r') then
	(debugf "" (rule_gen expr) (e1,e2);
	 debugf "renamed into " rule (l',r'));
      l', r';;

(*****************************************************************************)
(** termination problem *)
(*****************************************************************************)

type signature = {
  sig_typs : StrSet.t;
  sig_funs : typ StrMap.t;
  sig_vars : typ StrMap.t };;

type expr_problem = signature * expr_rule list;;
type problem = signature * rule list;;

let build_symb_typ_map s =
  verbose "build symbol-type map...";
  set_typ_consts s.sig_typs;
  StrMap.iter (fun id ty -> add_global_symb (Fun id) ty) s.sig_funs;
  StrMap.iter (fun id ty -> add_global_symb (Var id) ty) s.sig_vars;;

module Typ = struct type t = typ end;;
module TypOrd = Ord.Make (Typ);;
module TypMap = Map.Make (TypOrd);;

let inverse sm =
  StrMap.fold
    (fun s t tm ->
       TypMap.add t
	 (s :: try TypMap.find t tm with Not_found -> [])
	 tm)
    sm TypMap.empty;;

let typ_map b =
  TypMap.iter
    (fun t ss -> bprintf b "%a : %a;\n" (list ", " string) ss typ t);;

let signature b s =
  if s.sig_typs <> StrSet.empty then
    bprintf b "TYPES\n%a;\n\n" (set ", ") s.sig_typs;
  if s.sig_funs <> StrMap.empty then
    bprintf b "FUNS\n%a\n" typ_map (inverse s.sig_funs);
  if s.sig_vars <> StrMap.empty then
    bprintf b "VARS\n%a\n" typ_map (inverse s.sig_vars);;

let problem_gen rule b (s, rs) =
  signature b s;
  if rs <> [] then
    bprintf b "RULES\n%a;\n" (list ";\n" rule) (List.rev rs);;

let expr_problem = problem_gen expr_rule;;
let problem = problem_gen rule;;

let output_file_hot fn = output_file (Util.fprint problem) fn;;

(*****************************************************************************)
(** type of a term *)
(*****************************************************************************)

let typ_symb vmap = function
  | Var id ->
      (try StrMap.find id vmap with Not_found -> typ_of_ident id)
  | Fun id -> typ_of_ident id

let rec typ_head vmap = function
  | Symb s -> typ_symb vmap s
  | Abs (idtyps, t) ->
      let new_vmap =
	List.fold_left
	  (fun map (id, typ) -> StrMap.add id typ map)
	  vmap
	  idtyps in
      arrows (List.map snd idtyps) (typ_term new_vmap t)

and typ_term vmap (h, ts) =
  let typs, tid = typ_head vmap h in typ_app vmap typs tid ts

and typ_app vmap typs tid ts =
  match typs, ts with
    | _, [] -> typs, tid
    | typ :: typs', t :: ts' when typ = typ_term vmap t ->
	typ_app vmap typs' tid ts'
    | _, _ -> error "typing error";;

let typ_of = typ_term StrMap.empty;;

let preserve_typ_rule (t1, t2) = typ_of t1 = typ_of t2;;

let preserve_typ_rules = List.for_all preserve_typ_rule;;

(*****************************************************************************)
(** type constants necessary for typing a term *)
(*****************************************************************************)

let rec typ_consts_typ (ts, id) =
  List.fold_left
    (fun set typ -> StrSet.union set (typ_consts_typ typ))
    (StrSet.singleton id)
    ts;;

let typ_consts_symb s = typ_consts_typ (typ_of_ident (ident_of_symb s));;

let rec typ_consts_head = function
  | Symb s -> typ_consts_symb s
  | Abs (idtyps, t) ->
      List.fold_left
	(fun set (_, typ) -> StrSet.union (typ_consts_typ typ) set)
	(typ_consts t)
	idtyps

and typ_consts (h, ts) =
  StrSet.union (typ_consts_head h)
    (List.fold_left
       (fun set t -> StrSet.union set (typ_consts t))
       StrSet.empty
       ts);;

let typ_consts_rule (l, r) = StrSet.union (typ_consts l) (typ_consts r);;

let typ_consts_rules =
  List.fold_left
    (fun set r -> StrSet.union set (typ_consts_rule r)) StrSet.empty;;

(*****************************************************************************)
(** initial set of rules *)
(*****************************************************************************)

let get_rules, set_rules = get_set [];;

let compute_rules rs =
  verbose "compute rules...";
  set_rules (List.map rule_of_expr_rule rs);;

(*****************************************************************************)
(** set of symbols the termination of which remains to be proved *)
(*****************************************************************************)

let get_funs, set_funs = get_set StrSet.empty;;

(*****************************************************************************)
(** symbols that have been proved computable *)
(*****************************************************************************)

let get_comp_funs, set_comp_funs = get_set StrSet.empty;;

let add_comp_fun f =
  verbose (f ^ " is computable");
  set_comp_funs (StrSet.add f (get_comp_funs()));;

let add_comp_funs fs =
  verbosef "are computable: " (set ",") fs;
  set_comp_funs (StrSet.union fs (get_comp_funs()));;

let is_comp f = StrSet.mem f (get_comp_funs());;

(*****************************************************************************)
(** compute the map function ident -> rules and the set of undefined symbols *)
(*****************************************************************************)

let get_rules_map, set_rules_map = get_set StrMap.empty;;
let get_undef_funs, set_undef_funs = get_set StrSet.empty;;
let get_other_rules, set_other_rules = get_set [];;

let is_defined id = StrMap.mem id (get_rules_map());;

let rules_of_ident id =
  try StrMap.find id (get_rules_map()) with Not_found -> [];;

let rules_of_idents s =
  StrSet.fold (fun id rs -> rules_of_ident id @ rs) s [];;

let build_rules_map() =
  verbose "compute (un)defined function symbols...";
  (* we first set the set of undefined symbols as the set of all symbols *)
  set_undef_funs
    (StrMap.fold
       (fun id (sy,_) s -> match sy with Fun _ -> StrSet.add id s | Var _ -> s)
       (get_symb_typ_map())
       StrSet.empty);
  (* then, for each rule, we remove the top symbol of its LHS from the
     set of undefined symbols and add the rule in the map for defined
     symbols *)
  List.iter
    (fun (((h, _), _) as lr) ->
       match h with
	 | Symb (Fun id) ->
	     set_undef_funs (StrSet.remove id (get_undef_funs()));
	     set_rules_map
	       (StrMap.add id (lr :: rules_of_ident id) (get_rules_map()));
	     set_funs (StrSet.add id (get_funs()))
	 | Symb (Var _) | Abs _ ->
	     set_other_rules (lr :: get_other_rules()))
    (get_rules());
  (* undefined symbols are computable *)
  add_comp_funs (get_undef_funs());
  verbosef "undefined function symbols: " (set ",") (get_undef_funs());
  verbosef "defined function symbols: "
    (map "" (fun _ _ -> ()) ",") (get_rules_map());;

(*****************************************************************************)
(** definition (quasi-)ordering on defined symbols: f >= g if g is a
    defined symbol occuring in some defining rule of f *)
(*****************************************************************************)

let get_def_prec, set_def_prec = get_set StrPrec.empty;;

let def_cmp f g = StrPrec.cmp (get_def_prec()) f g;;
let fid_le f g = StrPrec.le (get_def_prec()) f g;;
let fid_lt f g = StrPrec.lt (get_def_prec()) f g;;
let fid_eq f g = StrPrec.eq (get_def_prec()) f g;;

let compute_def_prec =
  List.fold_left
    (fun p ((h, us), r) ->
       match h with
	 | Symb (Fun id) ->
	     (*IMPROVE: exclude rules of the form f .. r .. -> r *)
	     if List.mem r us then p
	     else
	       fold_funs
		 (fun id' _ts p ->
		    if id <> id' && is_defined id' then StrPrec.add id Ge id' p
		    else p)
		 p r
	 | Symb (Var _) | Abs _ -> p)
    StrPrec.empty;;
(*REMARK: because add_rule only adds constraints of the form Ge, the
  resulting precedence has only constraints of the form Eq, Ge and, by
  symmetry, Le *)

(** add symbols that do not depend on other symbols (they are not
    recorded by compute_def_prec) *)
let add_self_dep prec =
  StrMap.fold (fun f _ p -> StrPrec.add_elt f p) (get_rules_map()) prec;;

let build_def_prec() =
  verbose "compute definition ordering...";
  let prec = compute_def_prec (get_rules()) in
  (*verbosef "compute_def_prec:\n" (StrPrec.prec string) prec;*)
  let prec = StrPrec.strict prec in
  (*verbosef "strict:\n" (StrPrec.prec string) prec;*)
  let prec = add_self_dep prec in
  (*verbosef "add_self_prec:\n" (StrPrec.prec string) prec;*)
  verbosef "definition ordering:\n" (StrPrec.prec string) prec;
  set_def_prec prec;;

(** tell if a function symbol is recursively defined *)
let is_rec f =
  try
    List.iter
      (fun ((_, us), r) ->
	 (*IMPROVE: exclude rules of the form f .. r .. -> r *)
	 if not (List.mem r us) then
	   iter_funs
	     (fun g _ts ->
		match def_cmp f g with
		  | Gt | Un -> ()
		  | Ge | Eq | Le | Lt -> raise Exit)
	     r)
      (rules_of_ident f);
    false
  with Exit -> true;;

(** symbols smaller or equivalent to f *)
let funs_le fs =
  StrMap.fold
    (fun g _ gs -> if StrSet.exists (fid_le g) fs then StrSet.add g gs else gs)
    (get_rules_map()) (StrSet.union fs (get_undef_funs()));;

(*****************************************************************************)
(** compute the set of constructors and the map typ ident ->
    constructors, where a constructor is any symbol used in the lhs of a
    rule but the top symbol *)
(*****************************************************************************)

let get_cons_map, set_cons_map = get_set StrMap.empty;;
let get_cons, set_cons = get_set StrSet.empty;;

let is_cons cid = StrSet.mem cid (get_cons());;

let cons tid =
  try StrMap.find tid (get_cons_map()) with Not_found -> StrSet.empty;;

let compute_cons_set_map =
  List.fold_left (* rules *)
    (fun (cs, m) ((_h, ts), _r) ->
       List.fold_left (* lhs arguments *)
         (fun (cs, m) t ->
            fold_funs
              (fun id _ts (cs, m) ->
                 let _typs, typid = typ_of_ident id in
                 let cons = try StrMap.find typid m
                            with Not_found -> StrSet.empty in
                   StrSet.add id cs, StrMap.add typid (StrSet.add id cons) m)
              (cs, m)
              t)
         (cs, m)
         ts)
    (StrSet.empty, StrMap.empty);;

let build_cons_set_map() =
  verbose "compute type-constructor map...";
  let cs, m = compute_cons_set_map (get_rules()) in
    set_cons cs; set_cons_map m;
    verbosef "constructor symbols: " (set ",") cs;;

(*****************************************************************************)
(** map tid -> list of pairs (f, tys) such that typ_of_ident f = tys, tid *)
(*****************************************************************************)

let get_typ_map, set_typ_map = get_set StrMap.empty;;

let compute_typ_map() =
  StrMap.fold
    (fun f (sy, (tys, tid)) m ->
       match sy with
	 | Fun _ -> ladd tid (f, tys) m
	 | Var _ -> m)
    (get_symb_typ_map())
    StrMap.empty;;

let build_typ_map() =
  verbose "compute type-symbol map...";
  set_typ_map (compute_typ_map());;

(*****************************************************************************)
(** build the various sets and maps *)
(*****************************************************************************)

let build_maps() =
  build_rules_map();
  build_def_prec();
  build_cons_set_map();
  build_typ_map();;
