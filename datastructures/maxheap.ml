(* maxheap.ml
 * Implements a max heap data structure using an array implementation.
 *)

open Core

module MaxHeap (Ord : ORDTY) : (HEAP with type elt = Ord.t) = struct

  type elt = Ord.t
  type t = elt option array * int

  exception Heap_Empty

  let empty () = (Array.create ~len:1 None, 0)

  let is_empty (_,n) = n = 0

  let size (_,n) = n

  let peek (h,_) = h.(0)

  let peek_exn (h,_) =
    match h.(0) with
    | Some x -> x
    | _ -> raise Heap_Empty

  let parent i = (i - 1) / 2
  let left i = (i * 2) + 1
  let right i = (i * 2) + 2

  let swap arr i j =
    let temp = arr.(i) in
    arr.(i) <- arr.(j);
    arr.(j) <- temp

  let insert (h,n) x =
    (* Check if h needs resizing. *)
    let h =
      if n < Array.length h (* Not at capacity yet. *)
      then h
      else Array.init (2 * n) ~f:(fun i -> if i < n then h.(i) else None) (* Double the size! *)
      in
    (* Insert x at the bottom of the heap. *)
    h.(n) <- Some x;
    (* Bubble x upwards. *)
    let i = ref n in
    let pi = ref (parent !i) in
    while !i > 0 && Option.compare Ord.compare h.(!i) h.(!pi) = 1 do
      (* x is not at the top, and it is larger than it's parent, so swap them. *)
      swap h !i !pi;
      i := !pi;
      pi := parent !i
    done;
    (h,n+1)

  let delete (h,n) =
    if n <= 1 then empty () else (
      (* Replace the top value with the last value, then erase the last value. *)
      h.(0) <- h.(n-1);
      h.(n-1) <- None;
      (* Bubble the top value downwards. *)
      let i = ref 0 in
      let li = ref 1 in
      let ri = ref 2 in
      while (
        let x = match h.(!i) with | None -> raise (Failure "Module error.") | Some x -> x in
        match (h.(!li),h.(!ri)) with
          | (None, None) -> false (* Already at the bottom! *)
          | (Some l, None) -> if Ord.compare l x = 1 then (swap h !i !li; i := !li; true) else false
          | (None, Some r) -> if Ord.compare r x = 1 then (swap h !i !ri; i := !ri; true) else false
          | (Some l, Some r) ->
            if Ord.compare l r = 1 (* TODO: There is some code redundancy here. How can we get rid of it? *)
            then if Ord.compare l x = 1 then (swap h !i !li; i := !li; true) else false
            else if Ord.compare r x = 1 then (swap h !i !ri; i := !ri; true) else false
        ) do
        li := left !i;
        ri := right !i
      done;
      (h,n-1)
    )
end
