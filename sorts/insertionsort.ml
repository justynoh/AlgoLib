(* insertionsort.ml
 * Implements the insertion sort algorithm ascribing to the SORT signature.
 * Worst-case time complexity: O(n^2).
 *)

open Core

module InsertionSort : SORT = struct

  (* Insert y into l. Assumes l is sorted.
   *)
  let rec insert l y =
    match l with
    | [] -> [y]
    | x::xs -> if y <= x then y::l else x::(insert xs y)

  (* Scan through the list once, inserting each element of the old list into its
   * correct position in the new list.
   *)
  let sort l = List.fold ~init:[] ~f:insert l

end
