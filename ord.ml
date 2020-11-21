(**
HOT, a Higher-Order Termination prover
See the COPYRIGHTS and LICENSE files.

- Frederic Blanqui, 2010-03-22
*)

open Lib;;
open Util;;

type 'a cmp = 'a -> 'a -> int;;

(*****************************************************************************)
(** lexicographic ordering *)
(*****************************************************************************)

let rec lex cmp l1 l2 =
  match l1, l2 with
    | [], [] -> 0
    | _, [] -> 1
    | [], _ -> -1
    | x1 :: l1', x2 :: l2' ->
	match cmp x1 x2 with
	  | 0 -> lex cmp l1' l2'
	  | n -> n;;

(*****************************************************************************)
(** multiset ordering *)
(*****************************************************************************)

let mul cmp l1 l2 =
  let l1' = remove_firsts l1 l2 and l2' = remove_firsts l2 l1 in
    if l1' = [] && l2' = [] then 0
    else if List.for_all
      (fun x2 -> List.exists (fun x1 -> cmp x1 x2 > 0) l1') l2' then 1
    else -1;;
