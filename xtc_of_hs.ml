module List = struct
  let filter_map f l = 
    List.rev 
      (List.fold_left (fun l x -> match f x with 
        | Some x -> x :: l
        | None -> l) [] l)
  include List
end

module type XTC_OF =
sig
  exception Not_supported of string

  val haskell : Hs_type.haskell -> Xtc.problem
(** convert the given Haskell value into a XTC problem *)
end

module Xtc_of : XTC_OF =
struct
  open Util
  open Xtc
  open Hs_type

  (* a new different value is generated for every "_" identifiers *)
  let fresh_id = 
    let r = ref 0 in
    fun () -> 
      let _ = incr r in
      Printf.sprintf "%d" !r

  exception Not_supported of string

  let alpha_eq ty1 ty2 = 
    (* NOTE for checking the alpha equivalence of 2 terms, 
       the arrow operator can be considered as the applicative operator. *)
    let atom = 
      let rec aux = function 
        | Atom s -> Term.var s
        | List [] -> assert false
        | List l -> 
            let rec aux2 = function
              | [] -> assert false
              | [x] -> x
              | x :: xs -> Term.app (aux2 xs) x in
            aux2 (List.rev_map aux l) in
      aux in
    let ty t = 
      let rec f = function
        | TyArrow (ty1, ty2) -> Term.app (f ty1) (f ty2)
        | TyBasic t -> atom t
        | TyApp (ty1, ty2) -> Term.app (f ty1) (f ty2) in
      let t = f t in
      Term.abs 
        (snd 
           (Term.fold_vars
              (fun id _ (set, l) -> 
                if StrSet.mem id set then
                  set, l
                else
                  StrSet.add id set, (id, Term.typ_const "") :: l)
              (StrSet.empty, []) 
              t))
        t in
    Term.alpha_eq (ty ty1) (ty ty2)

  let not_supported s = raise (Not_supported s)

  (** check that the Haskell program begins with
      {-# htermination (F :: ty) #-} 
      and contains one and only one declaration F which has type ty. *)
  let check_htermination = function
    | StartTerm (f, ty) :: l -> 
        (match List.filter (function TypeAnnot (f0, _) -> f = f0 | _ -> false) l with
          | [TypeAnnot (_, ty0)] -> 
              if alpha_eq ty ty0 then f, ty else
                not_supported "alpha checking failed"
          | _ -> not_supported "no annotation of starting function found (or too many annotations)")
    | _ -> not_supported "no starting function found"

  (** given a term [t], return a list of subterm that the concatenation
      with "->" permits to retrieve back the [t] *)
  let flatten_top_arrow = 
    let rec aux l = function
      | TyArrow (ty, tys) -> aux (ty :: l) tys
      | x -> x :: l in
    fun ty -> List.rev (aux [] ty)

  let concat_top_arrow = function
    | [] -> assert false
    | [x] -> x
    | l -> 
        match List.rev l with
          | x :: l -> List.fold_left (fun acc e -> TyArrow (e, acc)) x ((*List.rev*) l)
          | _ -> assert false

  (** fold the given list and collect free variables in declarations *)
  let collect_free_var map_ty _ sgn_fun _ = 
    StrMap.fold
      (fun s v acc -> 
        if List.exists (function s0, _ -> s = s0) sgn_fun then
          acc
        else
          (s, concat_top_arrow v) :: acc)
      map_ty []

  (** destruct the LHS with a given type ty and 
      return the atomic sub-term with their type in a map *)
  let construct_with_lhs = function 
    | List (Atom _ :: param), (_, ty) ->
        let rec aux map = function
          | Atom p :: ps, ty :: tys -> aux (StrMap.add p (flatten_top_arrow ty) map) (ps, tys)
          | List _ :: _, _ -> not_supported "pattern form at lhs"
          | [], ty -> map, ty
          | _, [] -> failwith "type error" in
        fun map -> aux map (param, flatten_top_arrow ty)
    | _ -> not_supported ""

(*
  (** collect all the identifiers present at RHS that are in particular applied *)
  let get_rhs_constructor = 
    let rec aux set = function
      | a :: ll -> 
          List.fold_left
            (fun set -> function 
              | Atom _ -> set
              | List l -> aux set l)
            (match a with
              | Atom s -> StrSet.add s set
              | List l -> aux set l)
            ll
      | [] -> set in

    function
      | Atom _ -> assert false
      | List l -> aux StrSet.empty l
*)

  let retrieve_conseq_decl s = 

    (** check that there is no other recursive declarations *)
    let rec check_blank = function
      | LhsRhs (List (Atom s0 :: _), _) :: ll -> 
          if s = s0 then
            Printf.kprintf not_supported "interleaving declaration %S" s 
          else
            check_blank ll
      | [] -> ()
      | LhsRhs _ :: _ -> not_supported ""
      | _ :: ll -> check_blank ll in

    (** collect the max of consecutive mutual recursive declaration *)
    let rec collect_all l = function
      | LhsRhs (List (Atom s0 :: _) as lhs, rhs) :: ll -> 
          if s = s0 then collect_all ((lhs, rhs) :: l) ll else let () = check_blank ll in l
      | [] -> l
      | LhsRhs _ :: _ -> not_supported ""
      | _ :: ll -> collect_all l ll in

    (** position to the first declaration found in the program *)
    let rec find_first = function
      | LhsRhs (List (Atom s0 :: _) as lhs, rhs) :: ll -> 
          if s = s0 then collect_all [lhs, rhs] ll else find_first ll
      | [] | LhsRhs _ :: _ -> not_supported ""
      | _ :: ll -> find_first ll in

    find_first

  (** construct the map with every term and their corresponding type 
      occuring at RHS *)
  let infer_rhs =
    let rec aux term ty_return map =
      match term with
        | List (Atom head :: l_param) ->
            let map, l_ty_param =
              List.fold_left
                (fun (map, l_ty) term -> 
                  let map, ty = aux term None map in
                  map, ty :: l_ty)
                (map, [])
                l_param in
            let ty_return = 
              match ty_return with
                | Some ty_return -> ty_return
                | None -> List.fold_left (fun l_ty _ -> List.tl l_ty) (StrMap.find head map) l_param in
            (let ty_head = List.rev l_ty_param @ ty_return in
             if StrMap.mem head map then
               let _ = assert (ty_head = StrMap.find head map) in
               map
             else
               StrMap.add head ty_head map), 
            concat_top_arrow ty_return
        | Atom var -> map, concat_top_arrow (StrMap.find var map)
        | List ([] | List _ :: _) -> assert false in
    fun term ty map -> aux term (Some ty) map

  let proceed_lhsrhs (head, ty) prog map = 
    List.fold_left (fun map (lhs, rhs) -> 
      let map, ty_return = construct_with_lhs (lhs, ty) map in
      let map, _ = infer_rhs rhs ty_return map in
      map
    ) map (retrieve_conseq_decl head prog)


  let haskell (Program l) =
    let htermination = check_htermination l in
    let map_ty = 
      proceed_lhsrhs
        (match htermination with List [Atom s], _ -> s, htermination | _ -> assert false) 
        l
        StrMap.empty in
    
    let lhs_rhs = 
      List.filter_map (function
        | LhsRhs (List lhs, List rhs) -> 
          let rec aux_hs = function
            | Atom s :: xs -> 
              TermFunapp 
                ((if s = "_" then not_supported "underscore pattern" (*fresh_id ()*) else s), 
                 List.map (function Atom s -> TermVar s | List l -> aux_hs l) xs)
            | _ -> assert false in
          Some (aux_hs lhs, aux_hs rhs)
        | _ -> None) l in
    let sgn = 
      let rec of_ty = function
        | TyArrow (t1, t2) -> TypeArrow (of_ty t1, of_ty t2)
        | TyBasic (Atom s) -> TypeBasic s
        | TyBasic _ | TyApp _ -> not_supported "type application" in
      let sgn_data = 
        let of_typeannot = function
          | Data ([Atom ty_name], l) ->
              Some (List.filter_map (function TyBasic (Atom s) -> Some (s, ([], TypeBasic (ty_name))) | _ -> None) l)
          | _ -> None in
        let t = List.filter_map of_typeannot l in
        List.flatten t in
      let sgn_fun = 
        let of_typeannot = function
          | TypeAnnot (List [Atom s], t) -> Some (s, ([], of_ty t))
          | LhsRhs (List (Atom s :: _), _) -> 
              (try Some (s, ([], of_ty (concat_top_arrow (StrMap.find s map_ty)))) with
                | Not_found -> None)
          | _ -> None in
        let t = List.filter_map of_typeannot l in
        StrMap.fold (fun s v l -> (s, v) :: l) (List.fold_left (fun map (s, v) -> StrMap.add s v map) StrMap.empty t) [] in
      let sgn_var = List.map (function s, t -> s, of_ty t) (collect_free_var map_ty sgn_data sgn_fun lhs_rhs) in
      SignHO (sgn_var, sgn_data @ sgn_fun) in

    Term, 
    ((List.map (fun (lhs, rhs) -> lhs, rhs, []) lhs_rhs, []), sgn, None, CTNone), 
    StratFull, 
    STNone, 
    StatusNone, 
    None
end

let parse_haskell_ic s = 
  Hs_parser.haskells Hs_lexer.main (Lexing.from_channel s)
