(**
HOT, a Higher-Order Termination prover
See the COPYRIGHTS and LICENSE files.

- Frederic Blanqui, 2010-03-15
*)

open Printf;;
open Util;;

(*****************************************************************************)
(** useful exception *)
(*****************************************************************************)

exception Found of string;;

let found s = raise (Found s);;

(*****************************************************************************)
(** printing messages or debugging information *)
(*****************************************************************************)

let buf = Buffer.create 10000;;

let print_buffer() = Buffer.output_buffer stderr buf;;

let verbose s = if get_verbose() then bprintf buf "%s\n" s;;

let warning s = verbose ("warning: " ^ s);;

let big_sep() =
  verbose "================================================================";;

let small_sep() =
  verbose "----------------------------------------------------------------";;

let verbosef s f x = if get_verbose() then bprintf buf "%s%a\n" s f x;;

let debug s = if get_debug() then bprintf buf "%s\n" s;;

let debugf s f x = if get_debug() then bprintf buf "%s%a\n" s f x;;

let debugv s res y = debugf s res y; y;;

let debug1 s arg res f x =
  let y = f x in debugf s (pair arg " = " res) (x, y); y;;

let debug1' s arg res f x = debugf s arg x; debug1 s arg res f x;;

let debug2 s arg1 arg2 res f x1 x2 =
  let y = f x1 x2 in
    debugf s (pair (pair arg1 " " arg2) " = " res) ((x1, x2), y); y;;

let debug2' s arg1 arg2 res f x1 x2 =
  debugf s (pair arg1 " " arg2) (x1, x2); debug2 s arg1 arg2 res f x1 x2;;

(*****************************************************************************)
(** exceptions raised by tactics *)
(*****************************************************************************)

exception Error;;

let error s = verbose s; raise Error;;

let errorf s f x = verbosef s f x; raise Error;;

let try_crit f = try f() with Error -> ();;

(*****************************************************************************)
(** exit function *)
(*****************************************************************************)

type result = Yes | No | Maybe;;

let string_of_result = function
  | Yes -> "YES"
  | No -> "NO"
  | Maybe -> "MAYBE";;

let result oc r = fprintf oc "%s" (string_of_result r);;

let quit r = printf "%a\n" result r; flush stdout; exit 0;;

(*****************************************************************************)
(** miscellaneous functions *)
(*****************************************************************************)

let xor b1 b2 = (b1 && not b2) || (not b1 && b2);;

let is_Some = function None -> false | Some _ -> true;;

let array sep f b = Array.iter (postfix sep f b);;

let is_set_get_set m init =
  let r = ref init and s = ref false in
    (fun () -> !s),
    (fun () -> if !s then !r else error (sprintf "no %s provided" m)),
    (fun v -> if !s then error (sprintf "%s already provided" m)
              else (r := v; s := true));;

(*****************************************************************************)
(** permutations *)
(*****************************************************************************)

(** given an array of (non-empty) arrays l, next l returns each time a
    new choice of elements x such that x.(i) in l.(i), or raise
    Not_found *)
let next l =
  let n = Array.length l in
  let k = Array.init n (fun i -> Array.length l.(i) - 1)
  and c = Array.init n (fun i -> if i=n-1 then -1 else 0) in
  let rec aux i =
    if i < 0 then raise Not_found
    else if c.(i) >= k.(i) then (c.(i) <- 0; aux (i-1))
    else c.(i) <- c.(i) + 1
  in
    fun () -> aux (n-1); Array.init n (fun i -> l.(i).(c.(i)));;

let iter_permut f l =
  let next = next l in
    try while true do f (next()) done with Not_found -> ();;

(*****************************************************************************)
(** iteration functions *)
(*****************************************************************************)

let rec iter k f x = if k <= 0 then x else iter (k-1) f (f x);;

let rec iter_opt k f x =
  if k <= 0 then x
  else match f x with
    | Some y -> iter_opt (k-1) f y
    | None -> x;;

let nf f =
  let rec aux x =
    match f x with
      | Some y -> aux y
      | None -> x
  in aux;;

(*****************************************************************************)
(** functions on StrSet.t and StrMap.t *)
(*****************************************************************************)

let set_of_list = List.fold_left (fun fs f -> StrSet.add f fs) StrSet.empty;;

let keys m = StrMap.fold (fun k _ s -> StrSet.add k s) m StrSet.empty;;

(** [adds m1 m2] adds the bindings of m1 in m2 *)
let adds m1 m2 = StrMap.fold StrMap.add m1 m2;;

let lfind s m = try StrMap.find s m with Not_found -> [];;

let ladd s x m = StrMap.add s (x :: lfind s m) m;;

(*****************************************************************************)
(** temporary files *)
(*****************************************************************************)

let get_temps, set_temps = get_set [];;

let new_temp prefix postfix =
  let tmp = Filename.temp_file prefix postfix in
    set_temps (tmp :: get_temps()); tmp

let new_temp_change_postfix fn newpostfix =
  match extension fn with
    | Some _ ->
	let fn = Filename.chop_extension fn in
	let tmp = fn ^ newpostfix in
	let tmp =
	  if Sys.file_exists tmp then
	    Filename.temp_file (Filename.basename fn ^ "_") newpostfix
	  else tmp
	in set_temps (tmp :: get_temps()); tmp
    | None -> error ("file " ^ fn ^ " has no extension");;

let remove_temps() =
  verbose "remove temporary files...";
  List.iter Sys.remove (get_temps());;

(*****************************************************************************)
(** external tools *)
(*****************************************************************************)

let output_file output fn x =
  verbose ("create file " ^ fn ^ " ...");
  let oc = open_out fn in
    output oc x;
    close_out oc;;

let output_file_xtc fn xtc =
  output_file Libxml.output_xml fn (Xml_of_xtc.problem xtc);;

let command s =
  verbose ("system call: " ^ s ^ " ...");
  let ec = Sys.command s in verbosef "exit code: " int ec; ec;;

let is_set_lp_solver, get_lp_solver, set_lp_solver =
  is_set_get_set "linear program solver" "";;

(*****************************************************************************)
(** equality on type constructors *)
(*****************************************************************************)

type 'a eq = 'a -> 'a -> bool;;

let option_eq eq p q =
  match p, q with
    | Some x, Some y -> eq x y
    | None, None -> true
    | _, _ -> false;;

(*****************************************************************************)
(** functions for managing tests *)
(*****************************************************************************)

let get_test_mode, set_test_mode = get_set_bool();;

let tests = ref [];;

let add_test s k v = if not v then tests := (s,k) :: !tests;;

let print_test (s,k) = printf "%s %d failed\n" s k;;

let print_tests() = List.iter print_test (!tests);;

(*****************************************************************************)
(** functions on lists *)
(*****************************************************************************)

let ml_list f b = bprintf b "[%a]" (list "; " f);;

(** intersection of two lists *)
let inter xs = List.filter (fun y -> List.mem y xs);;

(** variant of List.exists where the function takes also the position
    as argument *)
let exists_i f =
  let rec aux i = function
    | [] -> false
    | x :: xs -> f i x || aux (i+1) xs
  in aux 0;;

(** return a pair made of the list of the k first elements and the
    list of remaining elements. raise Invalid_arg if there are less
    than k elements. *)
let split k l =
  let rec aux acc k l =
    if k <= 0 then List.rev acc, l
    else match l with
      | [] -> invalid_arg "Lib.split"
      | x :: l' -> aux (x :: acc) (k-1) l'
  in aux [] k l;;

(** variant of List.exists checking for two elements satisfying f *)
let exists2 f =
  let rec aux = function
    | [] -> false
    | x :: l -> if f x then List.exists f l else aux l
  in aux;;

(** given a list of lists l1 .. ln, compute the list of all the lists
    l which 1st element is in l1, 2nd element is in l2, etc. *)
let rec enum = function
  | [] -> [[]]
  | l :: ls ->
      let sls = enum ls in
	List.fold_left
	  (fun xs li -> List.map (fun s -> li :: s) sls @ xs)
	  [] l;;

(** variant of List.fold_left taking the index of the element as argument *)
let fold_left_i f a bs =
  fst (List.fold_left (fun (a, i) xi -> (f a i xi, i+1)) (a, 0) bs);;

(** [find_Some f [x1;..;xn]] returns [f xi] if i is the smallest index
    such that [f xi] <> None. It returns None otherwise. It is an
    optimization of [List.find ((<>) None) (List.map f xs)]. *)
let find_Some f =
  let rec aux = function
    | [] -> None
    | x :: xs ->
	match f x with
	  | None -> aux xs
	  | a -> a
  in aux;;

(** [fold_Some f a [x1;..;xn]] returns [fst (f ai xi)] if i is the
    smallest index such that [fst (f ai xi)] <> None, a1=a,
    a_{i+1}=snd(f ai xi). It returns None otherwise. It is an
    optimization of [List.find ((<>) None) (fst (List.fold_left (fun
    (l, a) xi -> let v, a' = f a xi in (v :: l, a')) ([], a) xs))]. *)
let fold_Some f =
  let rec aux a = function
    | [] -> None
    | x :: xs ->
	match f a x with
	  | None, a' -> aux a' xs
	  | v, _ -> v
  in aux;;

(** extension to ['a list -> 'b list] of a function [f:'a -> 'b list] *)
let fold_app f = List.fold_left (fun l x -> f x @ l) [];;

(*****************************************************************************)
(** iteration functions on two lists *)
(*****************************************************************************)

(** variation of List.fold_left2 raising invalid_arg only if the first
    list is smaller then the second list *)
let fold_left2 f =
  let rec aux acc l m =
    match l, m with
      | _, [] -> acc
      | x :: l, y :: m -> aux (f acc x y) l m
      | _, _ ->
	  invalid_arg "Lib.fold_left2: first list smaller than second list"
  in aux;;

(** variation of List.iter2 raising invalid_arg only if the first list
    is smaller then the second list *)
let iter2 f =
  let rec aux l m =
    match l, m with
      | _, [] -> ()
      | x :: l, y :: m -> f x y; aux l m
      | _, _ ->
	  invalid_arg "Lib.iter2: first list smaller than second list"
  in aux;;

(** filter the second list by doing the tests with the first list.
    raise invalid_arg if the first list is smaller than the second list *)
let filter2 f = fold_left2 (fun zs x y -> if f x then y :: zs else zs) [];;

(** return the first element of second list such that the
    corresponding element of the first list satisfies the required
    condition. raise Not_found if there is no such element. raise
    invalid_arg if the first list is smaller than the second list *)
let first2 f =
  let rec aux xs ys =
    match xs, ys with
      | _, [] -> raise Not_found
      | x :: l, y :: m -> if f x then y else aux l m
      | _, _ -> invalid_arg "Lib.first2: first list smaller than second list"
  in aux;;

(** variation of List.for_all2 raising invalid_arg only if the first
    list is smaller then the second list *)

exception For_all2;;
(* new exception for not interferring with the ones used in f *)

let for_all2 f xs ys =
  try iter2 (fun x y -> if not (f x y) then raise For_all2) xs ys; true
  with For_all2 -> false;;

(*****************************************************************************)
(** printable types *)
(*****************************************************************************)

module type PRN = sig
  type t
  val print : t bprint
end;;

module StrPrn = struct
  type t = string
  let print = string
end;;

module IntPrn = struct
  type t = int
  let print = int
end;;

(*****************************************************************************)
(** memorization functions *)
(*****************************************************************************)

module type MEM = sig
  type key
  val memo : (key -> 'a) -> (key -> 'a)
  val memo_debugf : string -> 'a bprint -> (key -> 'a) -> (key -> 'a)
end;;

module Mem = struct

  module Make (S : Map.S) (P : PRN with type t = S.key) = struct

    type key = S.key;;

    let memo f =
      let m = ref S.empty in
	fun x ->
	  try S.find x !m
	  with Not_found -> let y = f x in m := S.add x y !m; y;;

    let memo_debugf s p f =
      let m = ref S.empty in
	fun x ->
	  try S.find x !m
	  with Not_found ->
	    let y = f x in
	      debugf s (pair (par P.print) " = " p) (x, y);
	      m := S.add x y !m; y;;

    end;;

end;;

module StrMem = Mem.Make (StrMap) (StrPrn);;
module IntMem = Mem.Make (IntMap) (IntPrn);;

(*****************************************************************************)
(** Getting fresh names.  Use [notin vs] to generate names not in
    [vs]. Each call to [fresh] generates a new name not belonging to
    the sets declared with [notin] and different from previous calls
    to [fresh]. [reset] resets the set of used variables to 0. *)
(*****************************************************************************)

let fresh, notin, reset =
  let used = ref StrSet.empty and k = ref 0 and ident = sprintf "x%d" in
  let use id = used := StrSet.add id !used; id in
  let rec aux() =
    let id = ident !k in
      if StrSet.mem id !used then (incr k; aux()) else use id
  in
    (fun id -> if StrSet.mem id !used then aux() else use id),
    (fun s -> used := StrSet.union s !used),
    (fun () -> used := StrSet.empty);;
