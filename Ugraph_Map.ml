(* ugraph.ml
 * Implements an adjacency map representation of weighted graphs.
 *)

open Core

module Graph_AdjMap : GRAPH =
  struct

    module IntMap = Map.Make(Int)

    type vtx = int
    type wt = float
    type t = bool * int * wt IntMap.t IntMap.t

    exception VertexOutOfBounds

    let empty d = (d, 0, IntMap.empty)

    let isdirected (d,_,_) = d

    let insertvertex (d,sz,g) = (d, sz + 1, IntMap.add_exn g ~key:sz ~data:IntMap.empty)

    let isvertex (_,sz,_) u = 0 <= u && u < sz

    let insertedge (d,sz,g) u v w =
      if not (isvertex (d,sz,g) u) then raise VertexOutOfBounds else
      if not (isvertex (d,sz,g) v) then raise VertexOutOfBounds else
      let newu = IntMap.set ~key:v ~data:w (IntMap.find_exn g u) in
      let newv = IntMap.set ~key:u ~data:w (IntMap.find_exn g v) in
      let dg = IntMap.set ~key:u ~data:newu g in
      (d, sz, if d then dg else IntMap.set ~key:v ~data:newv dg)

    let size (_,sz,_) = sz

    let weight (d,sz,g) u v =
      if not (isvertex (d,sz,g) u) then raise VertexOutOfBounds else
      if not (isvertex (d,sz,g) v) then raise VertexOutOfBounds else
      IntMap.find (IntMap.find_exn g u) v

    let neighborhood (d,sz,g) u =
      if not (isvertex (d,sz,g) u) then raise VertexOutOfBounds else
      IntMap.to_alist (IntMap.find_exn g u)

    let of_list d n es =
      (* initialize a new graph with n vertices and no edges *)
      let g = List.init ~f:(fun i -> (i, IntMap.empty)) n |> IntMap.of_alist_exn in
      List.fold ~f:(fun g (u,v,w) -> insertedge g u v w) ~init:(d,n,g) es

  end
