(**
HOT, a Higher-Order Termination prover
See the COPYRIGHTS and LICENSE files.

- Frederic Blanqui, 2011-09-29
*)

open Util;;
open Printf;;

type 'a matrix = 'a array array;;

let make = Array.make_matrix;;

let init n m f = Array.init n (fun i -> Array.init m (f i));;

let size a =
  let n = Array.length a in
    n, if n = 0 then 0 else Array.length a.(0);;

let nb_lines a = Array.length a;;
let nb_cols a = snd (size a);;

let get a i j = a.(i).(j);;

let set a i j v = a.(i).(j) <- v;;

let iter f = Array.iter (Array.iter f);;

let iteri f = Array.iteri (fun i -> Array.iteri (f i));;

let matrix elt b =
  Array.iter (fun ai -> Array.iter (prefix " " elt b) ai; bprintf b "\n");;

(** sum of a.(i).(k) * a.(k).(j) for k=0 to n *)

let sum_rel zero add mul a b n i j =
  let rec aux acc k =
    if k < 0 then acc
    else aux (add acc (mul a.(i).(k) b.(k).(j))) (k-1)
  in aux zero n;;

(** matrix multiplication *)

exception Incompatible;;

let mul zero add mul a b =
  let n, m' = size a and m, l = size b in
    if m' <> m then raise Incompatible
    else init n l (sum_rel zero add mul a b (m-1));;

(** diagonal *)

let diag a = Array.mapi (fun i ai -> ai.(i)) a;;
