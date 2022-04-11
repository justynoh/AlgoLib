(* SORT.mli
 * Defines the SORT signature for various types of sorts.
 *)

open Core

module type SORT = sig

  type elt

  (* sort l => ls
   * where ls contains the elements of l in sorted order.
   *)
  val sort : elt list -> elt list

end
