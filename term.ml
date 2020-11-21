(**
HOT, a Higher-Order Termination prover
See the COPYRIGHTS and LICENSE files.

- Frederic Blanqui, 2010-03-15
*)

open Printf;;
open Util;;
open Lib;;

(*****************************************************************************)
(** identifiers *)
(*****************************************************************************)

type ident = string;;

(*****************************************************************************)
(** simple types *)
(*****************************************************************************)

type typ = typ list * ident;;
(** ([t1; ..; tn], id) represents t1 => .. => tn => id *)

type typing = typ StrMap.t;;

let rec typ b (ts, id) =
  bprintf b "%a%s" (list "" (postfix " => " ptyp)) ts id

and ptyp b ((ts, _) as t) = if ts = [] then typ b t else par typ b t;;

let typ_const id = [], id;;
let arrow t (us, id) = t :: us, id;;
let arrows ts (us, id) = ts @ us, id;;

let is_base_typ = function
  | [], _ -> true
  | _ -> false;;

let arity_typ (ts, _) = List.length ts;;

let iter_typ f =
  let rec aux (typs, tid) = f tid; List.iter aux typs in aux;;

let fold_typ f =
  let rec aux x (typs, tid) = List.fold_left aux (f tid x) typs in aux;;

(* new exception for not interferring with the ones used in f *)
exception Exists_typ;;

let exists_typ f ty =
  try iter_typ (fun id -> if f id then raise Exists_typ) ty; false
  with Exists_typ -> true;;

let exists_typs f = List.exists (exists_typ f);;

let in_typ tid = exists_typ ((=) tid);;
let in_typs tid = exists_typs ((=) tid);;

(*****************************************************************************)
(** well-formed lambda terms: we know when an identifier is a variable
    or a function symbol *)
(*****************************************************************************)

type symb = Var of ident | Fun of ident;;

let ident_of_symb = function Var id | Fun id -> id;;

let bsymb b s = string b (ident_of_symb s);;

type term = head * terms
  (** (h, [t1; ..; tn]) represents the application (h t1 .. tn) *)
and head = Symb of symb | Abs of (ident * typ) list * term
  (** Abs ([x1,t1; .., xn,tn], u) represents \x1:t1. .. \xn:tn. u *)
and terms = term list;;

let symb s = Symb s, [];;
let var id = Symb (Var id), [];;
let func id = Symb (Fun id), [];;

let app (h, ts) u = h, ts @ [u];;
let apps (h, ts) us = h, ts @ us;;
let appfunc fid ts = apps (func fid) ts;;

let abs1 id typ = function
  | Abs (idtyps, t), [] -> Abs ((id, typ) :: idtyps, t), []
  | (Abs _, _ | Symb _, _) as t -> Abs ([id, typ], t), [];;

let abs idtyps u =
  if idtyps = [] then u
  else
    match u with
      | Abs (idtyps', t), [] -> Abs (idtyps @ idtyps', t), []
      | (Abs _, _ | Symb _, _) -> Abs (idtyps, u), [];;

let rec raw_term b t = pair raw_head ", " (ml_list raw_term) b t
and raw_head b = function
  | Symb s -> bprintf b "Symb %a" bsymb s
  | Abs (idtyps, t) -> bprintf b "Abs(%a, %a)"
      (ml_list (pair string "," typ)) idtyps praw_term t
and praw_term b ((h, ts) as t) =
  if ts = [] then raw_head b h else par raw_term b t;;

let rec term b t = pair head "" (list "" (prefix " " pterm)) b t
and head b = function
  | Symb s -> bsymb b s
  | Abs ([], t) -> term b t
  | Abs ((id, ty) :: idtyps, t) ->
      bprintf b "(%s:%a.%a)" id typ ty head (Abs (idtyps, t))
and pterm b ((h, ts) as t) = if ts = [] then head b h else par term b t;;

let fresh_vars s =
  let rec aux n = if n <= 0 then [] else var (fresh s) :: aux (n-1)
  in aux;;

let fresh_idtyps s = List.map (fun ty -> fresh s, ty);;

type rule = term * term;;
type rules = rule list;;

let rule_gen term = pair term " -> " term;;

let rule = rule_gen term;;

(*****************************************************************************)
(** examples of terms for tests *)
(*****************************************************************************)

let x = var "x";;
let y = var "y";;
let z = var "z";;

let a = func "a";;
let b = func "b";;

let s t = appfunc "s" [t];;

let f t u = appfunc "f" [t; u];;
let g t u = appfunc "g" [t; u];;
let h t u = appfunc "h" [t; u];;

let o = typ_const "o";;

let lam (h, ts) u =
  if ts <> [] then invalid_arg "Term.lam"
  else match h with
    | Symb (Var x) -> abs1 x o u
    | Symb (Fun _) | Abs _ -> invalid_arg "Term.lam";;

(*****************************************************************************)
(** fold on function symbols or free variables occuring in a term *)
(*****************************************************************************)

let fold_funs f =
  let rec aux x (h, ts) = List.fold_left aux (aux_head x ts h) ts
  and aux_head x ts = function
    | Symb s -> aux_symb x ts s
    | Abs (_, t) -> aux x t
  and aux_symb x ts = function
    | Fun id -> f id ts x
    | Var _ -> x
  in aux;;

let iter_funs f =
  let rec aux (h, ts) = aux_head ts h; List.iter aux ts
  and aux_head ts = function
    | Symb s -> aux_symb ts s
    | Abs (_, t) -> aux t
  and aux_symb ts = function
    | Fun id -> f id ts
    | Var _ -> ()
  in aux;;

let fold_vars f =
  let rec aux bv x (h, ts) = List.fold_left (aux bv) (aux_head bv x ts h) ts
  and aux_head bv x ts = function
    | Symb s -> aux_symb bv x ts s
    | Abs (idtyps, t) ->
        let bv' = List.fold_left (fun s (id, _) -> StrSet.add id s) bv idtyps in
          aux bv' x t
  and aux_symb bv x ts = function
    | Fun _ -> x
    | Var id -> if StrSet.mem id bv then x else f id ts x
  in aux StrSet.empty;;

let iter_vars f =
  let rec aux bv (h, ts) = aux_head bv ts h; List.iter (aux bv) ts
  and aux_head bv ts = function
    | Symb s -> aux_symb bv ts s
    | Abs (idtyps, t) ->
        let bv' = List.fold_left (fun s (id, _) -> StrSet.add id s) bv idtyps in
          aux bv' t
  and aux_symb bv ts = function
    | Fun _ -> ()
    | Var id -> if StrSet.mem id bv then () else f id ts
  in aux StrSet.empty;;

(*****************************************************************************)
(** symbols occuring in a term *)
(*****************************************************************************)

let funs = fold_funs (fun f _ fs -> StrSet.add f fs) StrSet.empty;;

let funs_terms =
  List.fold_left (fun fs t -> StrSet.union fs (funs t)) StrSet.empty;;

let funs_rule (l, r) = StrSet.union (funs l) (funs r);;

let funs_rules =
  List.fold_left (fun s r -> StrSet.union s (funs_rule r)) StrSet.empty;;

(* new exception for not interferring with the ones used in f *)
exception Exists_fun;;

let exists_fun f t =
  try iter_funs (fun id us -> if f id us then raise Exists_fun) t; false
  with Exists_fun -> true;;

let in_funs f = exists_fun (fun g _ -> g=f);;

(*****************************************************************************)
(** free variables of a term *)
(*****************************************************************************)

let vars = fold_vars (fun id _ts s -> StrSet.add id s) StrSet.empty;;

let vars_terms =
  List.fold_left (fun vs t -> StrSet.union vs (vars t)) StrSet.empty;;

let vars_rule (l, r) = StrSet.union (vars l) (vars r);;

let vars_rules =
  List.fold_left (fun set r -> StrSet.union set (vars_rule r)) StrSet.empty;;

let in_vars x =
  let rec aux (h, ts) =
    (match h with
      | Symb (Var id) -> id = x
      | Symb (Fun _) -> false
      | Abs (idtyps, t) -> not (List.mem_assoc x idtyps) && aux t)
    || List.exists aux ts
  in aux;;

let nb_occs id m = try StrMap.find id m with Not_found -> 0;;

let occs =
  fold_vars
    (fun id _ts m -> StrMap.add id (1 + nb_occs id m) m)
    StrMap.empty;;

(*****************************************************************************)
(** eta normal form of a term *)
(*****************************************************************************)

let rec eta = function
  | Abs ((id, typ) :: idtyps, ((h, t :: ts) as b)), [] ->
      let us, u = split_last t ts in
      let v = abs idtyps (h, us) in
        if not (in_vars id v) && eta u = (Symb (Var id), []) then eta v
        else abs1 id typ (eta (abs idtyps b))
  | (Abs _|Symb _) as h, ts -> eta_head h, List.map eta ts

and eta_head = function
  | (Symb _) as h -> h
  | Abs (idtyps, t) -> Abs (idtyps, eta t);;

(*****************************************************************************)
(** bound variables of a term *)
(*****************************************************************************)

let add_vars = List.fold_left (fun vs (id,_) -> StrSet.add id vs);;
let add_var_typs = List.fold_left (fun vm (id,ty) -> StrMap.add id ty vm);;

let is_bound_var vm t =
  let h, us = eta t in
    match h with
      | Symb (Var x) -> us = [] && StrMap.mem x vm
      | Symb (Fun _) | Abs _ -> false;;

let rec are_distinct_bound_vars bv = function
  | [] -> true
  | t :: ts ->
      begin match eta t with
        | Symb (Var id), [] ->
            StrSet.mem id bv
            && are_distinct_bound_vars (StrSet.remove id bv) ts
        | Symb (Var _), _
        | (Symb (Fun _)|Abs _), _ -> false
      end;;

(** remove ending distinct bound variables *)
let rec remove_distinct_bound_vars bv ((h, us) as l) =
  match us with
    | [] -> l
    | u0 :: u1n ->
        let us', un = split_last u0 u1n in
        let hn, usn = eta un in
          match hn with
            | Symb (Var x) ->
                if usn = [] && StrSet.mem x bv then
                  remove_distinct_bound_vars (StrSet.remove x bv) (h, us')
                else l
            | Symb (Fun _) | Abs _ -> l;;

(*****************************************************************************)
(** renamings *)
(*****************************************************************************)

type renaming = string StrMap.t;;

let brenaming = map " = " string ", ";;

let rename s x = try StrMap.find x s with Not_found -> x;;

(** rename bound variables by [fresh] variables *)
let rename_bound_vars =
  let rec aux vm (h, ts) = aux_head vm h, List.map (aux vm) ts
  and aux_head vm h =
    match h with
      | Symb (Fun _) -> h
      | Symb (Var x) ->
          (try Symb (Var (StrMap.find x vm)) with Not_found -> h)
      | Abs (idtyps, t) ->
          let idtyps, vm =
            List.fold_left
              (fun (idtyps, vm) ((id, ty) as x) ->
                 let newid = fresh id in
                   if id = newid then x :: idtyps, vm
                   else (newid, ty) :: idtyps, StrMap.add id newid vm)
              ([], vm) idtyps
          in Abs (List.rev idtyps, aux vm t)
  in aux StrMap.empty;;

(*****************************************************************************)
(** alpha-conversion *)
(*****************************************************************************)

let alpha_eq_bv =
  let rec alpha_eq_bv (s1, s2) (h1, us1) (h2, us2) =
    (match h1, h2 with
       | Symb (Fun f1), Symb (Fun f2) -> f1 = f2
       | Symb (Var x1), Symb (Var x2) -> rename s1 x1 = rename s2 x2
       | Abs (idtyps1, t1), Abs (idtyps2, t2) ->
           List.map snd idtyps1 = List.map snd idtyps2
        && alpha_eq_bv
        (List.fold_left2
           (fun (s1, s2) (x1,_) (x2,_) ->
              if x1 = x2 then s1, s2
              else let x = fresh "x" in StrMap.add x1 x s1, StrMap.add x2 x s2)
           (s1, s2) idtyps1 idtyps2)
        t1 t2
       | Symb (Fun _), Symb (Var _) | Symb (Var _), Symb (Fun _)
       | Symb _, Abs _ | Abs _, Symb _ -> false)
    && List.length us1 = List.length us2
    && List.for_all2 (alpha_eq_bv (s1, s2)) us1 us2 in
  fun s -> alpha_eq_bv (s, s);;

let alpha_eq t1 t2 =
  let v1 = vars t1 and v2 = vars t2 in
    StrSet.equal v1 v2 && (notin v1; alpha_eq_bv StrMap.empty t1 t2);;

(*let alpha_eq t u = debug2 "alpha_eq " pterm pterm bool alpha_eq;;*)

let test_alpha k t u b = add_test "alpha_eq" k (alpha_eq t u = b);;

test_alpha 1 x x true;;
test_alpha 2 x y false;;
test_alpha 3 (lam x x) (lam y y) true;;
test_alpha 4 (lam x x) (abs1 "y" (arrow o o) y) false;;
test_alpha 5 (lam x x) (lam y x) false;;
test_alpha 6 (lam x (lam x x)) (lam y (lam z z)) true;;
test_alpha 7 (lam x (lam x x)) (lam y (lam z y)) false;;
test_alpha 8 (lam x (lam y x)) (lam y (lam x x)) false;;

(*****************************************************************************)
(** higher-order application of a substitution on a term (bound
    variables are renamed if necessary) *)
(*****************************************************************************)

type subs = term StrMap.t;;

let single x t = StrMap.add x t StrMap.empty;;

let vars_subs s =
  StrMap.fold (fun _ t -> StrSet.union (vars t)) s StrSet.empty;;

let bsubs = pmap " = " pterm ", ";;

let rec subs s (h, ts) = apps (subs_head s h) (List.map (subs s) ts)

and subs_head s = function
  | Symb (Var x) as h -> (try StrMap.find x s with Not_found -> h, [])
  | Symb (Fun _) as h -> h, []
  | Abs (idtyps, t) ->
      let idtyps, s =
        List.fold_left
          (fun (idtyps, s) ((id, ty) as x) ->
             let newid = fresh id in
               if id = newid then x :: idtyps, StrMap.remove id s
               else (newid, ty) :: idtyps, StrMap.add id (var newid) s)
          ([], s) idtyps
      in abs (List.rev idtyps) (subs s t);;

let subs s t = notin (vars_subs s); subs s t;;

(** extend s by mapping vid to t. raise Exit if s maps vid to a term
    not alpha equivalent to t *)
let add_subs vid t s =
  let v = var vid in
    if t = v then s
    else
      try if alpha_eq (StrMap.find vid s) t then s else raise Exit
      with Not_found -> StrMap.add vid t s;;

(** return a substitution which maps every variable [x] of [v] to [fresh x] *)
let renaming v =
  let aux id s =
    let id' = fresh id in
      if id' = id then s else StrMap.add id (var id') s in
    StrSet.fold aux v StrMap.empty;;

(** rename free variables of t2 by variables [notin] the free
    variables of t1 *)
let renamed_away t1 t2 = subs (notin (vars t1); renaming (vars t2)) t2;;

(*****************************************************************************)
(** check if a term is algebraic *)
(*****************************************************************************)

let rec is_alg = function
  | Symb (Fun _), ts -> List.for_all is_alg ts
  | Symb (Var _), [] -> true
  | Symb (Var _), _ | Abs _, _ -> false;;

(*****************************************************************************)
(** check if a term is an higher-order pattern *)
(*****************************************************************************)

let is_pattern =
  let rec aux bv = function
    | Symb (Fun _), ts -> List.for_all (aux bv) ts
    | Symb (Var _), ts -> are_distinct_bound_vars bv ts
    | Abs (idtyps, t), [] -> aux (add_vars bv idtyps) t
    | Abs _, _ -> false
  in aux StrSet.empty;;

(*****************************************************************************)
(** check whether a rule is non-duplicating *)
(*****************************************************************************)

let is_non_dup (l, r) =
  let vl = occs l in
    try
      StrMap.iter (fun id k -> if nb_occs id vl < k then raise Exit) (occs r);
      true
    with Exit -> false;;

(*****************************************************************************)
(** check whether a term is linear *)
(*****************************************************************************)

let is_linear =
  let aux id _ts s = if StrSet.mem id s then raise Exit else StrSet.add id s in
    fun t ->
      try ignore (fold_vars aux StrSet.empty t); true with Exit -> false;;

(*****************************************************************************)
(** subterm relation and iterators taking bound variables into account *)
(*****************************************************************************)

type 'b bound_vars = 'b * (ident -> typ -> 'b -> 'b);;

let bound_vars_renaming = StrMap.empty,
  fun x _ rm -> StrMap.add x (fresh "x") rm;;

let bound_vars_typing = StrMap.empty,
  fun x ty tm -> StrMap.add x ty tm;;

let bound_vars = (StrMap.empty, StrMap.empty),
  fun x ty (rm, tm) -> StrMap.add x (fresh "x") rm, StrMap.add x ty tm;;

let bound_vars_unit = (), fun _ _ _ -> ();;

let fold_subterms (bv0, bvf) f =
  let rec aux bv a ((_, ts) as t) =
    List.fold_left (aux bv) (aux_app bv a t) ts
  and aux_app bv a ((h, ts) as t) =
    fst (List.fold_left
           (fun (a, us) t -> f a bv (h, List.rev us), t :: us)
           (aux_head bv (f a bv t) h, [])
           ts)
  and aux_head bv a = function
    | Symb _ -> a
    | Abs (idtyps, u) -> aux_abs bv a u idtyps
  and aux_abs bv a u = function
    | [] -> aux bv a u
    | (x, ty) :: idtyps ->
        let bv = bvf x ty bv in
          aux_abs bv (f a bv (Abs (idtyps, u), [])) u idtyps
  in aux bv0;;

let iter_subterms (bv0, bvf) f =
  let rec aux bv ((_h, ts) as t) =
    aux_app bv t; List.iter (aux bv) ts
  and aux_app bv ((h, ts) as t) =
    f bv t; if ts = [] then aux_head bv h else aux_app bv (h, remove_last ts)
  and aux_head bv = function
    | Symb _ -> ()
    | Abs (idtyps, u) -> aux_abs bv u idtyps
  and aux_abs bv u = function
    | [] -> aux bv u
    | (x, ty) :: idtyps ->
        let bv = bvf x ty bv in
          f bv (Abs (idtyps, u), []); aux_abs bv u idtyps
  in aux bv0;;

let exists_subterm b f l =
  try iter_subterms b (fun bv t -> if f bv t then raise Exit) l; false
  with Exit -> true;;

let is_subterm_eq r l =
  let vr = vars r and vl = vars l in
    StrSet.subset vr vl
    && (notin (vars l);
        exists_subterm bound_vars_renaming
          (fun rm t -> alpha_eq_bv rm r t) l);;

let test_subterm_eq k r l b = add_test "subterm_eq" k (is_subterm_eq r l = b);;

test_subterm_eq 1 (f x (s (s y))) (f x (s y)) false;;

(*let is_subterm_eq = debug2 "is_subterm_eq " pterm pterm bool is_subterm_eq;;*)

let is_subterm r l = is_subterm_eq r l && not (alpha_eq_bv StrMap.empty r l);;

(*****************************************************************************)
(** matching on algebraic terms *)
(*****************************************************************************)

let matching_alg typ_of =
  let rec aux s = function
    | [] -> s
    | ((hl, ls), (ht, ts)) :: p ->
        match hl, ht with
          | Symb (Fun f), Symb (Fun g) ->
              if f = g && List.length ls = List.length ts then
                aux s (List.combine ls ts @ p)
              else raise Exit
          | Symb (Var x), _ ->
              let nl = List.length ls and nt = List.length ts in
                if nt < nl then raise Exit
                else let us, ts = Lib.split (nt - nl) ts in
                let u = ht, us in
                  begin try
                    if alpha_eq (StrMap.find x s) u
                    then aux s (List.combine ls ts @ p)
                    else raise Exit
                  with Not_found ->
                    if typ_of (var x) = typ_of u then
                      aux (StrMap.add x u s) (List.combine ls ts @ p)
                    else raise Exit
                  end
          | Abs _, _ -> invalid_arg "Term.matching_alg"
          | Symb (Fun _), (Abs _|Symb (Var _)) -> raise Exit
  in fun l t -> aux StrMap.empty [(l, t)];;

let matching_alg typ_of t u =
  try Some (matching_alg typ_of t u) with Exit -> None;;

(** tests *)

let untyped_matching_alg = matching_alg (fun _ -> o);;

let is_matching_alg_ok l t =
  match untyped_matching_alg l t with
    | Some s -> alpha_eq (subs s l) t
    | None -> false;;

let test_matching_alg k l t =
  add_test "matching_alg" k (is_matching_alg_ok l t);;

test_matching_alg 1 (app x y) (s a);;
test_matching_alg 2 (app x y) (f a a);;
test_matching_alg 3 (f x y) (f a b);;
test_matching_alg 4 (f x x) (f a a);;

let test_non_matching_alg k l t =
  add_test "non_matching_alg" k (untyped_matching_alg l t = None);;

test_non_matching_alg 1 (app x y) a;;
test_non_matching_alg 2 (f x y) a;;
test_non_matching_alg 3 (f x y) (func "f");;
test_non_matching_alg 4 (f x y) (appfunc "f" [a]);;
test_non_matching_alg 5 (f x x) (f a b);;

(*****************************************************************************)
(** matching modulo alpha conversion *)
(*****************************************************************************)

let matching typ_term tm0 =
  let rec aux m1 m2 s p =
    (*DEBUG:debugf "matching "
      (pair (par brenaming) " "
         (pair (par brenaming) " "
            (pair (par bsubs) " "
               (ml_list (pair term "," term))))) (m1, (m2, (s, p)));*)
    match p with
    | [] -> s
    | ((hl, ls), (ht, ts)) :: p ->
        let nl = List.length ls and nt = List.length ts in
          if nt < nl then raise Exit
          else let us, ts = Lib.split (nt - nl) ts in
            aux_head m1 m2 s hl (ht, us) (fun () -> List.combine ls ts @ p)
  and aux_head ((rm1, tm1) as m1) ((rm2, tm2) as m2) s hl ((ht, us) as u) p =
    match hl, ht with
      | Symb (Fun f), Symb (Fun g) ->
          if f = g && us = [] then aux m1 m2 s (p())
          else raise Exit
      | Symb (Var x), _ ->
          (try
             let x' = StrMap.find x rm1 in
               (* x is a bound variable renamed into x' *)
               (*debug (x ^ " is bound");*)
               match ht with
                 | Symb (Var y) ->
                     (try
                       let y' = StrMap.find y rm2 in
                         (* y is a bound variable renamed into y' *)
                         (*debug (y ^ " is bound");*)
                         if x' = y' && us = [] then aux m1 m2 s (p())
                         else raise Exit
                      with Not_found -> raise Exit)
                 | Symb (Fun _) -> raise Exit
                 | Abs _ -> raise Exit
           with Not_found ->
             (* x is a free variable *)
             (*debug (x ^ " is free");*)
             (try
                if alpha_eq (StrMap.find x s) u then aux m1 m2 s (p())
                else raise Exit
              with Not_found ->
                (*debug (x ^ " not defined yet");*)
                let vu = vars u in
                  if StrMap.exists (fun z _ -> StrSet.mem z vu) rm2
                  then raise Exit
                  else if typ_term tm1 (var x) = typ_term tm2 u then
                    aux m1 m2 (StrMap.add x u s) (p())
                  else raise Exit))
      | Abs (idtyps_l, l), Abs (idtyps_t, t) ->
          let n = List.length idtyps_l in
            if List.length idtyps_t < n then raise Exit
            else
              let idtyps_t, idtyps = Lib.split n idtyps_t in
              let m1, m2 =
                List.fold_left2
                  (fun ((rm1, tm1), (rm2, tm2)) (x1, ty1) (x2, ty2) ->
                     if ty1 <> ty2 then raise Exit
                     else let x = fresh "x" in
                       (StrMap.add x1 x rm1, StrMap.add x1 ty1 tm1),
                       (StrMap.add x2 x rm2, StrMap.add x2 ty2 tm2))
                  (m1, m2) idtyps_l idtyps_t
              in aux m1 m2 s ((l, abs idtyps t) :: p())
      | Abs _, Symb _ | Symb (Fun _), (Abs _|Symb (Var _)) -> raise Exit
  in fun l t ->
    aux (StrMap.empty, StrMap.empty) (StrMap.empty, tm0) StrMap.empty [(l, t)];;

let matching typ_term tm0 l t =
  notin (StrSet.union (vars l) (vars t));
  try Some (matching typ_term tm0 l t) with Exit -> None;;

(** tests *)

let untyped_matching = matching (fun _ _ -> o) StrMap.empty;;

let is_matching_ok l t =
  match untyped_matching l t with
    | Some s -> alpha_eq (subs s l) t
    | None -> false;;

let test_matching k l t = add_test "matching" k (is_matching_ok l t);;

test_matching 1 (app x y) (s a);;
test_matching 2 (app x y) (f a a);;
test_matching 3 (f x y) (f a b);;
test_matching 4 (f x x) (f a a);;
test_matching 5 (lam x x) (lam y y);;
test_matching 6 (lam x y) (lam z a);;

let test_non_matching k l t =
  add_test "non_matching" k (untyped_matching l t = None);;

test_non_matching 1 (app x y) a;;
test_non_matching 2 (f x y) a;;
test_non_matching 3 (f x y) (func "f");;
test_non_matching 4 (f x y) (appfunc "f" [a]);;
test_non_matching 5 (f x x) (f a b);;
test_non_matching 6 (lam x x) a;;
test_non_matching 7 (lam x x) (lam y x);;
test_non_matching 8 (lam x y) (lam z z);;

(*****************************************************************************)
(** contexts and subterm iterators taking contexts and bound variables
    into account *)
(*****************************************************************************)

type context =
  | Cempty
  | CAbs of ident * typ * context
  | CAppRight of context * term
  | CAppMiddle of head * terms * context * terms;;

let rec raw_context b = function
  | Cempty -> bprintf b "Cempty"
  | CAbs (x, ty, c) -> bprintf b "CAbs(%s,%a,%a)" x typ ty raw_context c
  | CAppRight (c, t) -> bprintf b "CAppRight(%a,%a)" raw_context c term t
  | CAppMiddle (h, ts, c, us) -> bprintf b "CAppMiddle(%a,%a,%a,%a)"
      head h (ml_list term) ts raw_context c (ml_list term) us;;

let rec fill u = function
  | Cempty -> u
  | CAbs (id, ty, c) -> fill (abs1 id ty u) c
  | CAppRight (c, t) -> fill (app u t) c
  | CAppMiddle (h, ts, c, us) -> fill (h, ts @ u :: us) c;; 

let term_of_context = fill (func "[]");;

let context b c = term b (term_of_context c);;
let pcontext b c = pterm b (term_of_context c);;

let fold_app_cont f bv c a ((h, ts) as t) =
  let rts = List.rev ts in
  let _, c, a =
    List.fold_left
      (fun (rts, c, a) t ->
         let us = List.tl rts and d = CAppRight (c, t) in
           us, d, f a bv d (h, List.rev us))
      (rts, c, f a bv c t)
      rts
  in c, a;;

let fold_subterms_cont_gen fold_app (bv0, bvf) f =
  let rec aux bv c a ((h, ts) as t) =
    let a, _, _ =
      List.fold_left
        (fun (a, ts, us) t ->
           let vs = List.tl us in
             aux bv (CAppMiddle (h, List.rev ts, c, vs)) a t, t :: ts, vs)
        (aux_app bv c a t, [], ts)
        ts
    in a
  and aux_app bv c a ((h, _) as t) =
    let c, a = fold_app f bv c a t in aux_head bv c a h
  and aux_head bv c a = function
    | Symb _ -> a
    | Abs (idtyps, u) -> aux_abs u bv c a idtyps
  and aux_abs u bv c a = function
    | [] -> aux bv c a u
    | (x, ty) :: idtyps ->
        let bv = bvf x ty bv and d = CAbs (x, ty, c) in
          aux_abs u bv d (f a bv d (Abs (idtyps, u), [])) idtyps
  in aux bv0 Cempty;;

let fold_subterms_cont b = fold_subterms_cont_gen fold_app_cont b;;

let iter_app_cont f bv c ((h, ts) as t) =
  f bv c t;
  let rts = List.rev ts in
  let _, c =
    List.fold_left
      (fun (rts, c) t ->
         let us = List.tl rts and d = CAppRight (c, t) in
           f bv d (h, List.rev us); us, d)
      (rts, c)
      rts
  in c;;

let iter_subterms_cont_gen iter_app (bv0, bvf) f =
  let rec aux bv c ((h, ts) as t) =
    aux_app bv c t;
    ignore
      (List.fold_left
         (fun (ts, us) t ->
            let vs = List.tl us in
              aux bv (CAppMiddle (h, List.rev ts, c, vs)) t;
              t :: ts, vs)
         ([], ts)
         ts)
  and aux_app bv c ((h, _) as t) = aux_head bv (iter_app f bv c t) h
  and aux_head bv c = function
    | Symb _ -> ()
    | Abs (idtyps, u) -> aux_abs u bv c idtyps
  and aux_abs u bv c = function
    | [] -> aux bv c u
    | (x, ty) :: idtyps ->
        let bv = bvf x ty bv and d = CAbs (x, ty, c) in
          f bv d (Abs (idtyps, u), []); aux_abs u bv d idtyps
  in aux bv0 Cempty;;

let iter_subterms_cont b = iter_subterms_cont_gen iter_app_cont b;;

let exists_subterm_cont b f t =
  try iter_subterms_cont b (fun bv c t -> if f bv c t then raise Exit) t; false
  with Exit -> true;;

(*****************************************************************************)
(** beta-reduction and rewriting (assuming that rhs variables are
    included in lhs variables) *)
(*****************************************************************************)

type 'a reduct = typing -> context -> 'a -> 'a option;;

let union f g tm c r =
  match f tm c r with
    | Some _ as r -> r
    | None -> g tm c r;;

let beta_weak_hd _ c (h, ts) =
  match h, ts with
    | Abs ((x, _) :: idtyps, u), t :: ts ->
        Some (fill (apps (subs (single x t) (abs idtyps u)) ts) c)
    | (Abs _|Symb _), _ -> None;;

let beta_hd _ c (h, ts) =
  match h, ts with
    | Abs ((x, _) :: idtyps, u), [t] ->
        Some (fill (subs (single x t) (abs idtyps u)) c)
    | (Abs _|Symb _), _ -> None;;

let rewrite_hd typ_term rs tm c t =
  Lib.find_Some
    (fun (l, r) ->
       match matching typ_term tm l t with
         | Some s -> Some (fill (subs s r) c)
         | None -> None)
    rs;;

type rewrite =
  | Beta
  | Rewrite of rule;;

let rewrite b = function
  | Beta -> bprintf b "beta-reduces"
  | Rewrite r -> bprintf b "rewrites with rule {%a}" rule r;;

type trace = (term * context * rewrite) list;;

let step b (t, c, r) =
  bprintf b "%a, in context %a, %a to\n" pterm t pcontext c rewrite r;;

let trace b l = list "" step b (List.rev l);;

let loop b (t, l) = bprintf b "%a%a" trace l pterm t;;

let trace_beta_hd _tm c ((h, ts) as t, tr) =
  match h, ts with
    | Abs ((x, _) :: idtyps, u), [v] ->
        Some (fill (subs (single x v) (abs idtyps u)) c,
              (fill t c, c, Beta) :: tr)
    | (Abs _|Symb _), _ -> None;;

let trace_rewrite_hd typ_term rs tm c (t, tr) =
  Lib.find_Some
    (fun ((l, r) as lr) ->
       match matching typ_term tm l t with
         | Some s -> Some (fill (subs s r) c, (fill t c, c, Rewrite lr) :: tr)
         | None -> None)
    rs;;

(*****************************************************************************)
(** reduct(s) of a term at any position given a function saying if
    there is a reduct at the top *)
(*****************************************************************************)

exception Reduct of term;;

let reduct f t =
  try
    iter_subterms_cont bound_vars_typing
      (fun tm c u ->
         match f tm c u with
           | Some r -> raise (Reduct r)
           | None -> ())
      t;
    None
  with Reduct r -> Some r;;

type 'a reducts = 'a reduct -> 'a -> 'a list;;

let reducts f t =
  fold_subterms_cont bound_vars_typing
    (fun l tm c u -> (*debugf "reducts " (pair pcontext " " pterm) (c, t);*)
       match f tm c u with
         | Some r -> r :: l
         | None -> l)
    [] t;;

let trace_reducts f (t, tr) =
  fold_subterms_cont bound_vars_typing
    (fun l tm c u -> (*debugf "reducts " (pair pcontext " " pterm) (c, t);*)
       match f tm c (u, tr) with
         | Some r -> r :: l
         | None -> l)
    [] t;;

(*****************************************************************************)
(** tests *)
(*****************************************************************************)

let oo = arrow o o;;
let cn = arrow oo oo;;

let church k = abs ["f",oo;"x",o] (iter k (app (var "f")) x);;

let nb_iter f x =
  let rec aux k = function
    | Symb (Var y), [] when y = x -> k
    | Symb (Var g), [t] when g = f -> aux (k+1) t
    | (Symb (Var _|Fun _)|Abs _), _ -> raise Exit
  in fun u -> try Some (aux 0 u) with Exit -> None;;

let nat_of_head = function
  | Abs ([f,tf;x,tx], u) when tf = oo && tx = o -> nb_iter f x u
  | Abs _ | Symb _ -> None;;

let nat (h, ts) = if ts = [] then nat_of_head h else None;;

let test_nat k = add_test "nat" k (nat (church k) = Some k);;

test_nat 0;;
test_nat 1;;
test_nat 2;;
test_nat 10;;

(*DEBUG:let rec term b t = pair head "" (list "" (prefix " " pterm)) b t

and head b h =
  match nat_of_head h with
    | Some k -> int b k
    | None ->
        match h with
          | Symb s -> bsymb b s
          | Abs ([], t) -> term b t
          | Abs ((id, _ty) :: idtyps, t) ->
              bprintf b "(%s.%a)" id (*typ ty*) head (Abs (idtyps, t))

and pterm b ((h, ts) as t) = if ts = [] then head b h else par term b t;;*)

let add = abs ["k",cn;"l",cn;"f",oo;"x",o]
  (apps (var "k") [var "f"; apps (var "l") [var "f";x]]);;

let test_beta k t u =
  add_test "beta" k (alpha_eq (nf (reduct beta_hd) t) u);;

test_beta 1 (apps add [church 1; church 1]) (church 2);;
test_beta 2 (apps add [church 2; church 2]) (church 4);;

let e = func "0";;
let p x y = appfunc "+" [x; y];;
let m x y = appfunc "*" [x; y];;

let rec peano k = if k <= 0 then e else s (peano (k-1));;

let rs = [p e y, y;
          p (s x) y, p x (s y);
          m e y, e;
          m (s x) y, p y (m x y)];;

let test_rewrite k t u =
  add_test "rewriting" k
    (alpha_eq (nf (reduct (rewrite_hd (fun _ _ -> o) rs)) t) u);;

test_rewrite 1 (p (m (peano 3) (p (peano 1) (peano 2))) (peano 2)) (peano 11);;

(*****************************************************************************)
(** mgu of a list of pairs of algebraic terms *)
(*****************************************************************************)

let mgus =
  let compat x t s = try t = StrMap.find x s with Not_found -> true in
  let rec aux s = function
    | [] -> Some s
    | x :: tl ->
        match x with
          | (Symb (Fun f), ts), (Symb (Fun g), us)
              when f = g (*&& List.length ts = List.length us*) ->
              aux s (List.fold_left2 (fun l t u -> (t,u)::l) tl ts us)
          | (Symb (Var x), []), (Symb (Var y), [])
              when x = y -> aux s tl
          | (Symb (Var x), []), t | t, (Symb (Var x), [])
              when not (in_vars x t) && compat x t s ->
              let xt = single x t in
              let sxt (l,r) = subs xt l, subs xt r in
                aux (StrMap.add x t (StrMap.map (subs xt) s)) (List.map sxt tl)
          | ((Symb (Var _|Fun _)|Abs _), _),
                ((Symb (Var _|Fun _)|Abs _), _) -> raise Exit
  in fun l -> try aux StrMap.empty l with _ -> None;;

let mgu t u = mgus [t,u];;

(* tests *)

let test_mgu k l sopt =
  add_test "mgu" k (option_eq (StrMap.equal (=)) sopt (mgus l));;

test_mgu 1 [a, b] None;;
test_mgu 2 [a, a] (Some StrMap.empty);;
test_mgu 3 [f x (s x), a] None;;
test_mgu 4 [f x (s x), f a (s b)] None;;
test_mgu 5 [f x (s y), f x (s y)] (Some StrMap.empty);;
test_mgu 6 [f x (s y), f (s x) (s y)] None;;
test_mgu 7 [x, y; x, z]
  (Some (StrMap.add "x" z (StrMap.add "y" z StrMap.empty)));;

(*****************************************************************************)
(** check whether a left-algebraic system is orthogonal *)
(*****************************************************************************)

let iter_fun_subterms f =
  let rec aux ((h, ts) as t) =
    match h with
      | Symb (Fun _) -> f t
      | Symb (Var _) | Abs _ -> ();
    List.iter aux ts
  in aux;;

let not_overlay rs =
  try
    List.iter
      (fun (l1, r1) ->
         List.iter
           (fun (l2, r2) ->
              (*FIXME: use equality modulo renaming of variable rules*)
              if l1 <> l2 || r1 <> r2 then
                iter_fun_subterms
                  (fun sl2 ->
                     match mgu l1 sl2 with
                       | Some _ -> raise Exit
                       | None -> ())
                  (renamed_away l1 l2)
           )
           rs)
      rs;
    true
  with Exit -> false;;

let is_orthogonal_alg rs =
  List.for_all (fun (l, _) -> is_linear l) rs && not_overlay rs;;

(* tests *)

let test_ortho k rs b = add_test "ortho" k (is_orthogonal_alg rs = b);;

test_ortho 1 [] true;;
test_ortho 2 [s x, x] true;;
test_ortho 3 [f a x, x; f (s x) y, f x (s y)] true;;
test_ortho 4 [f x x, x] false;;
test_ortho 5 [f a x, x; f (s x) y, f x (s y); f (s (s x)) y, y] false;;
