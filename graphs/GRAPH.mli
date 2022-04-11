open Core

module type GRAPH =
  sig

    type vtx = int
    type edge

    type t

    exception VertexOutOfBounds

    (* empty => G
     * where G is the empty graph.
     *)
    val empty : t

    (* insertvertex G => G'
     * where, if G = (V,E), then G' = (V u {(size G)}, E).
     *)
    val insertvertex : t -> t

    (* isvertex G u => b
     * where b is true iff 0 <= u < |G|.
     *)
    val isvertex : t -> vtx -> bool

    (* insertedge G e => G'
     * where, if G = (V,E), then G' = (V, E u {e}).
     *)
    val insertedge : t -> edge -> t

    (* size G => n
     * where n = |G|.
     *)
    val size : t -> int

  end
