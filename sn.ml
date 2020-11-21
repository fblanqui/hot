(**
HOT, a Higher-Order Termination prover
See the COPYRIGHTS and LICENSE files.

- Frederic Blanqui, 2010-03-18
*)

open Expr;;
open Lib;;
open Util;;

let try_prove_termin() =
  big_sep(); verbose "try to prove termination...";
  if get_other_rules() <> [] then
    verbose "don't know how to prove the termination of rules \
      with non function-headed lhs"
  else begin
    Fo.compute_fo_funs();
    Fo.try_prove_fo_part();
    if StrSet.is_empty (get_funs()) (*&& get_other_rules() = []*) then
      (verbose "no remaining rule"; quit Yes);
    try_crit Size.try_sbt
  end;;
