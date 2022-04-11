open Core

module type UGRAPH =
  sig

    include GRAPH

    type edge

    type t

    exception VertexOutOfBounds

    (* mkedge u v => e
     * where e represents the undirected edge {u,v}.
     *)
    val mkedge : vtx -> vtx -> edge

    (* isedge G u v => b
     * where, if G = (V,E), b is true iff {u,v} in E.
     *)
    val isedge : t -> vtx -> vtx -> bool

    (* neighborhood G u => L
     * where L = {v : {u,v} in E}.
     *)
    val neighborhood : t -> vtx -> vtx list

    (* of_list n es => G
     * where G = (V,E) is an undirected graph on n vertices with edges es. In
     * particular, V = [0,n), E = {{u,v} : (u,v) in es}. Requires for all (u,v)
     * in es, 0 <= u,v < n, and no duplicate edges exist in es.
     *)
    val of_list : bool -> int -> (vtx * vtx) list -> t

  end
