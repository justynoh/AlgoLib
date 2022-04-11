(* mergesort.ml
 * Implements the merge sort algorithm ascribing to the SORT signature.
 * Worst-case time complexity: O(n log n).
 *)

open Core

module MergeSort (Ord : ORDTY) : (SORT with type elt = Ord.t) = struct

  type elt = Ord.t

  (* Merges two sorted lists into a single sorted list.
   *)
  let rec merge l1 l2 =
    match (l1,l2) with
    | ([],_) -> l2
    | (_,[]) -> l1
    | (x::xs, y::ys) -> if Ord.compare x y = -1 then x::(merge xs l2) else y::(merge l1 ys)

  (* Splits l into two lists (l1,l2) of equal size such that l is a permutation
   * of l1 @ l2.
   *)
  let rec split l =
    match l with
    | [] -> ([],[])
    | _::[] -> (l,[])
    | x::y::xs -> let (l1,l2) = split xs in (x::l1, y::l2)

  (* Divide the list, recursively sort the two sublists, then merge them back.
   *)
  let rec sort l =
    match l with
    | [] -> []
    | _::[] -> l
    | _ -> let (l1,l2) = split l in merge (sort l1) (sort l2)

end
