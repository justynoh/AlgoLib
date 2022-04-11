(* heap.mli
 * Defines the HEAP signature for heaps.
 *)

open Core

module type HEAP = sig

  type elt
  type t

  exception Heap_Empty

  (* empty () => h
   * where h is an empty heap.
   *)
  val empty : unit -> t

  (* is_empty h => b
   * where b is true iff h is empty.
   *)
  val is_empty : t -> bool

  (* size h => n
   * where n = |h|.
   *)
  val size : t -> int

  (* peek h => o
   * where o is None if h is empty, and is Some x otherwise, where x is the
   * element at the top of the heap.
   *)
  val peek : t -> elt option

  (* peek_exn h => x
   * raises Heap_Empty if the heap is empty, and otherwise returns x, where x is
   * the element at the top of the heap.
   *)
  val peek_exn : t -> elt

  (* insert h x => h'
   * where h' is a valid heap, and h' = h u {x}.
   *)
  val insert : t -> elt -> t

  (* delete h => h'
   * where h' is a valid heap with the top element of h removed.
   *)
  val delete : t -> t

end
