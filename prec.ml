(**
HOT, a Higher-Order Termination prover
See the COPYRIGHTS and LICENSE files.

- Frederic Blanqui, 2010-03-17
*)

open Printf;;
open Lib;;
open Util;;

(*****************************************************************************)
(** type for ordering constraints *)
(*****************************************************************************)

type comp = Le | Lt | Eq | Gt | Ge | Un;;

let string_of_comp = function
  | Le -> "<="
  | Lt -> "<"
  | Eq -> "="
  | Gt -> ">"
  | Ge -> ">="
  | Un -> "?";;

let comp b c = bprintf b "%s" (string_of_comp c);;

let opp = function
  | Le -> Ge
  | Lt -> Gt
  | Eq -> Eq
  | Gt -> Lt
  | Ge -> Le
  | Un -> Un;;

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
  type t = comp EltMap.t EltMap.t
  val cmp : t -> elt -> elt -> comp
  val le : t -> elt -> elt -> bool
  val lt : t -> elt -> elt -> bool
  val eq : t -> elt -> elt -> bool
  val find : elt -> t -> comp EltMap.t
  val fold : (elt -> elt -> comp -> 'b -> 'b) -> t -> 'b -> 'b
  val iter : (elt -> elt -> comp -> unit) -> t -> unit
  val fold_elt : elt -> (elt -> comp -> 'b -> 'b) -> t -> 'b -> 'b
  val elts : t -> elt -> comp -> EltSet.t
  val prec : elt bprint -> t bprint
  val empty : t
  val add : elt -> comp (* <> Un *) -> elt -> t -> t
  val add_elt : elt -> t -> t
  val merge : t -> t -> t
  val compare : t -> t -> int
  val strict : t -> t
  val large : t -> t
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
  with type t = comp OM.t OM.t
= struct

  type elt = O.t;;

  module EltMap = OM;;
  module EltSet = OS;;

  (** Sets of constraints are represented by using finite maps of type
      [elt -> elt -> comp] such that the sets of pairs [(x,y)] such that
      - [comp prec x y = Eq] is an equivalence relation
      - [comp prec x y = Ge|Gt|Le|Lt] is a quasi-ordering
      In addition, [comp prec x y = opp (comp prec y x)] *)

  type t = comp EltMap.t EltMap.t;;

  let con b (x, c, y) = bprintf b "%a %a %a" OP.print x comp c OP.print y;;

  let cmp prec x y =
    try EltMap.find y (EltMap.find x prec)
    with Not_found -> if x = y then Eq else Un;;

  let le prec x y =
    match cmp prec x y with
      | Le | Lt | Eq -> true
      | Ge | Gt | Un -> false;;

  let lt prec x y = cmp prec x y = Lt;;

  let eq prec x y = cmp prec x y = Eq;;

  let find x prec = try EltMap.find x prec with Not_found -> EltMap.empty;;

  let fold f = EltMap.fold (fun x m b -> EltMap.fold (f x) m b);;

  let iter f = EltMap.iter (fun x m -> EltMap.iter (f x) m);;

  let fold_elt x f prec = EltMap.fold f (find x prec);;

  (** elements [y] such that [cmp x y = c] *)
  let elts prec x c =
    fold_elt x
      (fun y d s -> if d = c then EltSet.add y s else s)
      prec
      EltSet.empty;;

  let prec elt b prec =
    let elts = EltSet.elements
      (EltMap.fold (fun x _ s -> EltSet.add x s) prec EltSet.empty) in
      (*DEBUG:EltSet.elements
	(fold
	   (fun x y _ set -> EltSet.add x (EltSet.add y set))
	   prec
	   EltSet.empty) in*)
    list "" (prefix "\t" elt) b elts;
    List.iter
      (fun x ->
	 bprintf b "\n%a\t%a"
	   elt x
	   (list "\t" (fun b y -> comp b (cmp prec x y))) elts)
      elts;;

  let empty = EltMap.empty;;

  let raw_add x c y prec = EltMap.add x (EltMap.add y c (find x prec)) prec;;

  let sym_raw_add x c y prec =
    (*debugf "sym_raw_add: " con (x, c, y);*)
    if x = y then prec else raw_add x c y (raw_add y (opp c) x prec);;

  let sym_raw_adds xs c ys prec =
    List.fold_left
      (fun prec x' ->
	 List.fold_left
	   (fun prec y' -> sym_raw_add x' c y' prec)
	   prec
	   ys)
      prec
      xs

  type elts =
      { lt : elt list; le : elt list; eq : elt list;
	ge : elt list; gt : elt list };;

  let belts =
    let f = list "," OP.print in
      fun b x ->
	bprintf b "{ lt = %a; le = %a; eq = %a; ge = %a; gt = %a }"
	  f x.lt f x.le f x.eq f x.ge f x.gt;;

  let elts_none = { lt = []; le = []; eq = []; ge = []; gt = [] };;

  let add_elt y c elts =
    match c with
      | Lt -> { elts with lt = y :: elts.lt }
      | Le -> { elts with le = y :: elts.le }
      | Eq -> { elts with eq = y :: elts.eq }
      | Ge -> { elts with ge = y :: elts.ge }
      | Gt -> { elts with gt = y :: elts.gt }
      | Un -> invalid_arg "Prec.add_elt";;

  let raw_elts prec x =
    try EltMap.fold add_elt (EltMap.find x prec) elts_none 
    with Not_found -> elts_none;;

  let add_le x y prec =
    (*debugf "add_le " (pair OP.print " " OP.print) (x, y);*)
    match cmp prec x y with
      | Gt -> raise Incompatible
      | Le | Lt | Eq -> prec
      | Ge -> (* we already have x >= y. adding x <= y makes x = y *)
	  let elts_x = raw_elts prec x and elts_y = raw_elts prec y in
	  let xeq = x :: elts_x.eq and yeq = y :: elts_y.eq in
	    (*debugf "elts_x = " belts elts_x;
	    debugf "elts_y = " belts elts_y;*)
	    (* the elements = to x become = to the elements = to y *)
	    sym_raw_adds xeq Eq yeq
	      (* the elements >= to y become >= to the elements = to x *)
	      (sym_raw_adds (yeq @ elts_y.le) Ge elts_x.ge
		 (* the elements > to y become > to the elements = to x *)
		 (sym_raw_adds (yeq @ elts_y.lt) Gt elts_x.gt
		    prec))
      | Un ->
	  let elts_x = raw_elts prec x and elts_y = raw_elts prec y in
	    (*debugf "elts_x = " belts elts_x;
	      debugf "elts_y = " belts elts_y;*)
	    (*elements <= than x are <= than the elements >= than y*)
	  let smaller_than_x = x :: elts_x.eq @ elts_x.ge
	  and bigger_than_y = y :: elts_y.eq @ elts_y.le in
	    sym_raw_adds smaller_than_x Le bigger_than_y
	      (sym_raw_adds smaller_than_x Lt elts_y.lt
		 (sym_raw_adds elts_x.gt Lt (elts_y.lt @ bigger_than_y)
		    prec));;

  let add_lt x y prec =
    match cmp prec x y with
      | Gt | Ge | Eq -> raise Incompatible
      | Lt -> prec
      | Le | Un ->
	  let elts_x = raw_elts prec x
	  and elts_y = raw_elts prec y in
	    sym_raw_adds
	      (elts_x.eq @ elts_x.ge @ elts_x.gt) Lt (elts_y.eq @ elts_y.lt)
	   (sym_raw_add x Lt y prec);;

  let add_ge x y = add_le y x;;
  let add_gt x y = add_lt y x;;

  let add_eq x y prec = add_le x y (add_ge x y prec);;

  let add = function
    | Eq -> add_eq
    | Ge -> add_ge
    | Le -> add_le
    | Lt -> add_lt
    | Gt -> add_gt
    | Un -> invalid_arg "Prec.add:Un";;

  let add x c y p = add c x y p;;
    (*DEBUG:if x<>y then debugf "add " (postfix ":" con) (x,c,y);
    let p = add c x y p in
      if x<>y then debugf "" (prec OP.print) p;
      p;;*)

  let add_elt x p = if EltMap.mem x p then p else EltMap.add x EltMap.empty p;;

  let merge = fold (fun x y c prec -> add x c y prec);;

  (* total ordering on t *)
  let compare = EltMap.compare (EltMap.compare Pervasives.compare);;

  let strict p =
    fold (fun x y c p ->
	    match c with
	      | Le -> raw_add x Lt y p
	      | Ge -> raw_add x Gt y p
	      | Lt | Gt | Eq -> raw_add x c y p
	      | Un -> p)
      p empty;;

  let large p =
    fold (fun x y c p ->
	    match c with
	      | Le | Ge -> raw_add x Eq y p
	      | Lt | Gt | Eq -> raw_add x c y p
	      | Un -> p)
      p empty;;

  (** list of elements in increasing order using depth-first search *)
  let linear p =
    (* vs memorizes the nodes already visited *)
    let rec aux ((vs, l) as vsl) x =
      if EltSet.mem x vs then vsl
      else
	let elts_x = raw_elts p x in
	let xeq = x :: elts_x.eq in
	let vs = List.fold_left (fun vs x -> EltSet.add x vs) vs xeq in
	let vs, l = List.fold_left aux (vs, l) elts_x.lt in
	  vs, xeq :: l in
    let _, l = EltMap.fold (fun x _ vsl -> aux vsl x) p (EltSet.empty, []) in
      l;;

  let filter f p =
    EltMap.fold
      (fun x m p ->
	 if f x then EltMap.add x (EltMap.filter (fun y _ -> f y) m) p else p)
      p EltMap.empty;;

end;;

(*****************************************************************************)
(** finite quasi-orderings on strings *)
(*****************************************************************************)

module StrPrec = Make(StrOrd)(StrMap)(StrSet)(StrPrn);;
