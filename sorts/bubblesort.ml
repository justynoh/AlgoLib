(* bubblesort.ml
 * Implements the bubble sort algorithm ascribing to the SORT signature.
 * Worst-case time complexity: O(n^2).
 *)

open Core

module BubbleSort : SORT = struct

  (* Scans through the list once, looking at each pair of elements. If the
   * elements are in the wrong order, swap them, then continue the scan. Also
   * outputs whether a swap has occurred in the pass. Requires O(n) time.
   *)
  let rec bubble_onepass = function
    | x::y::xs ->
      if x > y
      then let (xxs,_) = bubble_onepass (x::xs) in (y::xxs, true)
      else let (yxs,swap) = bubble_onepass (y::xs) in (x::yxs, swap)
    | xs -> (xs, false)

  (* Continuously scan through the list until no more swaps are needed over an
   * entire pass. At most n passes are needed (in the case that the input list
   * is reverse-sorted).
   *)
  let rec sort l =
    let (lpassed,swap) = bubble_onepass l in
    if swap then sort lpassed else lpassed

end
