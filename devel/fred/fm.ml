(*****************************************************************************)
(** Fourier-Motzkin variable elimination procedure *)
(*****************************************************************************)

(** A set of constraints is represented by a map id1 -> id2 -> k
    representing a constraint id1 <= id2+k. Hence, a variable id1 is
    mapped to the unique expression id2+k that is bigger than id1. For
    doing FME, we need the inverse map id1 -> id2 -> k representing
    the constraint id1 >= id2+k. *)

(** {id1 >= id2+k, id1 >= id2+l} is equivalent to {id1 >= id2+max{k,l}} *)
let raw_add_ge id1 id2 k cs =
  try let m = StrMap.find id1 cs in
    try
      if k > StrMap.find id2 m then StrMap.add id1 (StrMap.add id2 k m) cs
      else cs
    with Not_found -> StrMap.add id1 (StrMap.add id2 k m) cs
  with Not_found -> StrMap.add id1 (StrMap.add id2 k StrMap.empty) cs;;

(** expressions smaller than id *)
let inverse1 id cs =
  fold (fun id1 id2 k ges -> (* id1 <= id2 + k <-> id2 >= id1 - k *)
	  if id2 <> id then ges
	  else try
	    if -k > StrMap.find id1 ges then StrMap.add id1 (-k) ges
	    else ges
	  with Not_found -> StrMap.add id1 (-k) ges)
    cs StrMap.empty;;

(** full inversion of a set of constraints *)
let inverse cs = fold raw_add_ge cs StrMap.empty;;

(** remove all constraints on id *)
let remove_id id cs =
  StrMap.fold
    (fun id1 m cs ->
       if id1 = id then cs
       else let m = StrMap.remove id m in
	 if m = StrMap.empty then cs else StrMap.add id1 m cs)
    cs StrMap.empty;;

(** eliminate the variable id *)
let fme_step id cs =
  let m1 = inverse1 id cs
  and m2 = try StrMap.find id cs with Not_found -> StrMap.empty in
    StrMap.fold
      (fun id1 k cs -> (* id1+k <= id *)
	 StrMap.fold
	   (fun id2 l cs -> (* id <= id2+l *)
	      raw_add id1 id2 (l-k) cs)
	   m2 cs)
      m1 (remove_id id cs);;

let fme_step id cs =
  debugf "eliminate " string id;
  let cs = fme_step id cs in debugf "" cons cs; cs;;

(** choose an id not in ids *)
let choose_id ids cs =
  try iter
    (fun id1 id2 _k ->
       if not (StrSet.mem id1 ids) then found id1
       else if not (StrSet.mem id2 ids) then found id2
       else raise Not_found)
    cs; raise Not_found
  with Found id -> id;;

(** eliminate all variables not in ids *)
let simpl ids =
  let rec aux cs =
    try aux (fme_step (choose_id ids cs) cs) with Not_found -> cs
  in aux;;

let size_vars =
  List.fold_left
    (fun ids (_, _, sz) ->
       match sz with
	 | Succ (_, id) -> StrSet.add id ids
	 | Inf -> ids)
    StrSet.empty;;
