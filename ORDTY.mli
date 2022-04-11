(* ORDTY.mli
 * Defines a signature for ordered types.
 *)

open Core

module type ORDTY = sig

  type t

  (* compare x y => c
   * where if x < y then c = -1, if x = y then c = 0 and if x > y then c = 1.
   *)
  val compare : t -> t -> int

end
