(* sort.mli
 * Defines the SORT signature for various types of sorts.
 *)

open Core

module type SORT = sig

  (* sort l => ls
   * where ls contains the elements of l in sorted order.
   *)
  val sort : int list -> int list

end
