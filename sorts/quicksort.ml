(* quicksort.ml
 * Implements the quick sort algorithm ascribing to the SORT signature.
 * Average-case time complexity: O(n log n).
 * Worst-case time complexity: O(n^2).
 *)

open Core

module QuickSort : SORT = struct

  (* Pick a random pivot and split the list about the pivot. Then, recursively
   * sort the two halves. Requires |l| = n.
   *)
  let rec quicksort l n =
    match l with
    | [] -> []
    | _::[] -> l
    | _ -> (
      let pivotidx = Random.int n in
      let pivot = List.nth_exn l pivotidx in
      let (lt,ltn,gt,gtn) = List.foldi
        ~init:([],0,[],0)
        ~f:(fun i (lt,ltn,gt,gtn) x ->
          if i = pivotidx then (lt,ltn,gt,gtn) else (* Ignore the pivot. *)
          if x < pivot then (x::lt,ltn+1,gt,gtn) else (lt,ltn,x::gt,gtn+1))
        l in
      (quicksort lt ltn) @ pivot :: (quicksort gt gtn)
      )

  (* Call the quicksort helper, which requires the length of the input list.
   *)
  let sort l = quicksort l (List.length l)

end
