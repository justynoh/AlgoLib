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
     * where, if G = (V,E) and v in V, then G' = (V - v, E \ E(v)), and
     * otherwise, G' = G.
     *)
    val delete_vtx : t -> vtx -> t

    (* delete_vtx_exn G v => G'
     * where, if G = (V,E) and v in V, then G' = (V - v, E \ E(v)), and
     * and otherwise, raises Vtx_Not_Found.
     *)
    val delete_vtx_exn : t -> vtx -> t

    (* is_vtx G v => b
     * where b is true if v is a vertex in G.
     *)
    val is_vtx : t -> vtx -> bool

    (* set_vtx_label G v vl => G'
     * where, if v in G, then  G' is the graph G, with vertex v now having label
     * vl, and otherwise, G' = G.
     *)
    val set_vtx_label : t -> vtx -> vtx_label -> t

    (* set_vtx_label_exn G v vl => G'
     * where, if v in G, then G' is the graph G, with vertex v now having label
     * vl, and otherwise, raises Vtx_Not_Found.
     *)
    val set_vtx_label_exn : t -> vtx -> vtx_label -> t

    (* get_vtx_label G v => o
     * where, if v in G, o = Some vl where vl is the label of vertex v in G, and
     * otherwise, o = None.
     *)
    val get_vtx_label : t -> vtx -> vtx_label option

    (* get_vtx_label_exn G v => vl
     * where, if v in G, then vl is the label of vertex v in G, and otherwise,
     * raises Vtx_Not_Found.
     *)
    val get_vtx_label_exn : t -> vtx -> vtx_label

    (*** Edge operations ***)

    (* add_edge G e el => G'
     * where, if G = (V,E) and e not in E, then G' = (V, E + e) with label el,
     * otherwise,
     *
    val add_edge : t -> edge -> edge_label -> t *)

    (* delete_edge G e => G'
     * where, if G = (V,E) and e in E, then G' = (V, E - e), and otherwise,
     * G' = G.
     *)
    val delete_edge : t -> edge -> t

    (* delete_edge_exn G e => G'
     * where, if G = (V,E) and e in E, then G' = (V, E - e), and otherwise,
     * raises Edge_Not_Found.
     *)
    val delete_edge_exn : t -> edge -> t

    (* is_edge G e => b
     * where b is true if e is an edge in G.
     *)
    val is_edge : t -> edge -> bool

    (* set_edge_label G e el => G'
     * where, if e in G, then G' is the graph G, with edge e now having label
     * el, and otherwise, G' = (V, E + e) with label el.
     *)
    val set_edge_label : t -> edge -> edge_label -> t

    (* set_edge_label_exn G e el => G'
     * where, if e in G, then G' is the graph G, with edge e now having label
     * el, and otherwise, raises Edge_Not_Found.
     *
    val set_edge_label_exn : t -> edge -> edge_label -> t *)

    (* get_edge_label G e => o
     * where, if e in G, then o = Some el where el is the label of edge e in G,
     * and otherwise, o = None.
     *)
    val get_edge_label : t -> edge -> edge_label option

    (* get_edge_label_exn G e => el
     * where, if e in G, then el is the label of edge e in G, and otherwise,
     * raises Edge_Not_Found.
     *)
    val get_edge_label_exn : t -> edge -> edge_label

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
