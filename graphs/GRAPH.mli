(* GRAPH.mli
 * An interface for a graph structure allowing for creating, reading, deleting
 * and updating vertices and edges in a graph. Both directed and undirected
 * graphs are supported. Multigraphs are not supported.
 *)

open Core

module type GRAPH =
  sig

    type vtx = int
    type edge = vtx * vtx
    type vtx_label
    type edge_label
    type directed = bool

    type t

    exception Vtx_Not_Found
    exception Edge_Not_Found

    (* init d => G
     * where G is the empty graph with the given directedness.
     *)
    val init : directed -> t

    (* is_directed G => d
     * where d is the directedness of graph G.
     *)
    val is_directed : t -> directed

    (* size G => n
     * where n = |G|.
     *)
    val size : t -> int

    (*** Vertex operations ***)

    (* add_vtx G vl => (G', v)
     * where, if G = (V,E), then G' = (V + v, E) with label vl.
     *)
    val add_vtx : t -> vtx_label -> t * vtx

    (* delete_vtx G v => G'
     * where, if G = (V,E), then G' = (V - v, E \ E(v)).
     *)
    val delete_vtx : t -> vtx -> t

    (* is_vtx G v => b
     * where b is true if v is a vertex in G.
     *)
    val is_vtx : t -> vtx -> bool

    (* set_vtx_label G v vl => G'
     * where G' is the graph G, with vertex v now having label vl.
     *)
    val set_vtx_label : t -> vtx -> vtx_label -> t

    (* get_vtx_label G v => vl
     * where vl is the label of vertex v in G.
     *)
    val get_vtx_label : t -> vtx -> vtx_label

    (*** Edge operations ***)

    (* add_edge G e el => G'
     * where, if G = (V,E), then G' = (V, E + e) with label el.
     *)
    val add_edge : t -> edge -> edge_label -> t

    (* delete_edge G e => G'
     * where, if G = (V,E), then G' = (V, E - e).
     *)
    val delete_edge : t -> edge -> t

    (* is_edge G e => b
     * where b is true if e is an edge in G.
     *)
    val is_edge : t -> edge -> bool

    (* set_edge_label G e el => G'
     * where G' is the graph G, with edge e now having label el.
     *)
    val set_edge_label : t -> edge -> edge_label -> t

    (* get_edge_label G e => el
     * where el is the label of edge e in G.
     *)
    val get_edge_label : t -> edge -> edge_label

    (*** Graph queries ***)

    (* neighborhood G u => es
     * where es = {(v,el) : (u,v) in E, el = edge label of (u,v)}.
     *)
    val neighborhood : t -> vtx -> (vtx * edgelabel) list

    (* of_list n es => G
     * where G is a graph containing n vertices and edges in es.
     *)
    val of_list : directedness -> int -> edge list -> t

  end
