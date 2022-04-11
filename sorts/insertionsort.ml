(* insertionsort.ml
 * Implements the insertion sort algorithm ascribing to the SORT signature.
 * Worst-case time complexity: O(n^2).
 *)

open Core

module InsertionSort (Ord : ORDTY) : (SORT with type elt = Ord.t) = struct

  type elt = Ord.t

  (* Insert y into l. Assumes l is sorted.
   *)
  let rec insert l y =
    match l with
    | [] -> [y]
    | x::xs -> if Ord.compare y x = 1 then x::(insert xs y) else y::l

  (* Scan through the list once, inserting each element of the old list into its
   * correct position in the new list.
   *)
  let sort l = List.fold ~init:[] ~f:insert l

end
