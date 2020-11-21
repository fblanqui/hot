(**
HOT, a Higher-Order Termination prover
See the COPYRIGHTS and LICENSE files.

- Frederic Blanqui, 2011-09-28
*)

open Util;;
open Printf;;
open Matrix;;

(** relation on arguments *)
type rel = Lt | Eq | Un;;

let string_of_rel = function
  | Lt -> "<"
  | Eq -> "="
  | Un -> "?";;

let rel b r = string b (string_of_rel r);;

(** addition (or) *)
let add r s =
  match r, s with
    | Un, s -> s
    | r, Un -> r
    | Eq, Eq -> Eq
    | Lt, (Lt|Eq) | Eq, Lt -> Lt;;

(** multiplication (and) *)
let mul r s =
  match r, s with
    | Un, _ | _, Un -> Un
    | Eq, Eq -> Eq
    | Lt, (Lt|Eq) | Eq, Lt -> Lt;;

(** call matrices *)
module CM = struct type t = string * string * rel matrix end;;
module CMOrd = Ord.Make (CM);;
module CMSet = Set.Make (CMOrd);;

let call_matrix b (f, g, a) = bprintf b "%s -> %s:\n%a" f g (matrix rel) a;;

let call_matrices b = CMSet.iter (call_matrix b);;

(** call matrix sets composition *)
let comp cs1 cs2 =
  CMSet.fold
    (fun (g, h, b) cs ->
       CMSet.fold
	 (fun (f, g', a) cs ->
	    if g' <> g then cs
	    else
	      try CMSet.add (f, h, Matrix.mul Un add mul b a) cs
	      with Incompatible -> cs)
	 cs2 cs)
    cs1 CMSet.empty;;

(** tell if a call matrix set is complete *)
let is_complete cs = CMSet.subset (comp cs cs) cs;;

(** completion of a call matrix set *)
let completion cs0 =
  let rec aux cs =
    let cs' = CMSet.union cs (comp cs cs0) in
      if CMSet.equal cs' cs then cs else aux cs'
  in aux cs0;;

(** recursion behavior of a symbol f in a complete call matrix set *)

module RB = struct type t = rel array end;;
module RBOrd = Ord.Make (RB);;
module RBSet = Set.Make (RBOrd);;

let rec_behav b = Array.iter (prefix " " rel b);;

let rb_of f m = try StrMap.find f m with Not_found -> RBSet.empty;;

let rec_behavs cs =
  CMSet.fold
    (fun (f, g, c) m ->
       if f = g then StrMap.add f (RBSet.add (diag c) (rb_of f m)) m
       else m)
    cs StrMap.empty;;

(** find a lexical order *)

let find rb =
  let rec aux i =
    if i < 0 then raise Not_found
    else
      if RBSet.exists (fun r -> r.(i) = Lt) rb
	&& RBSet.for_all (fun r -> r.(i) <> Un) rb
      then i
      else aux (i-1)
  in aux;;

(* we assume that 0 <= i < n *)
let remove r i =
  let n = Array.length r in
    Array.append (Array.sub r 0 i) (Array.sub r (i+1) (n-1-i));;

let lex_order rb =
  let rec aux rb n acc =
    try
      let i = find rb (n-1) in
      let rb' = RBSet.fold
	(fun r rb -> if r.(i) = Lt then rb else RBSet.add (remove r i) rb)
	rb RBSet.empty
      in aux rb' (n-1) (i::acc)
    with Not_found -> List.rev acc
  in try aux rb (Array.length (RBSet.choose rb)) []
    with Not_found -> [];;

let lex_orders cs = StrMap.map lex_order (rec_behavs cs);;
