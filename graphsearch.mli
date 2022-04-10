(* ugraph.ml
 * Implements an adjacency map representation of weighted undirected graphs.
 *)

open Core

module type GRAPHSEARCH =
  sig

    include GRAPH

    (* dfs init onvisit compare ontraversal onrevisit onfinish g
     * Performs a dfs on the graph g starting from the lowest-numbered vertex in
     * each connected component. The initial value of the accumulator is init.
     * Within the dfs in each connected component, onvisit is called with the
     * accumulator when a vertex is visited for the first time. Neighbors are
     * visited in the order determined by compare (see List.sort). If the
     * neighbor has already been visited, onrevisit is called. Otherwise,
     * ontraversal is called and then the dfs continues from the neighbor. Once
     * all neighbors have been visited for a vertex, onfinish is called. Returns
     * (x, parent, dfstimes) where x is the final value of the accumulator,
     * parent u is the parent of u in the dfs tree or None if it is a root,
     * dfstimes u is the (start, finish) times of u during the dfs.
     *)
    val dfs : init:('a)
              -> onvisit:('a -> vtx -> 'a)
              -> compare:(vtx -> vtx * wt -> vtx * wt -> int)
              -> ontraversal:('a -> vtx -> vtx -> wt -> 'a)
              -> onrevisit:('a -> vtx -> vtx -> wt -> 'a)
              -> onfinish:('a -> vtx -> 'a)
              -> t
              -> 'a * (vtx -> vtx option) * (vtx -> int * int)

    (* dfs1 init onvisit compare ontraversal onrevisit onfinish g v
     * Performs a dfs on g starting from v for only that connected component.
     * Returns (x, parent, dfstimes) where x is the final value of the
     * accumulator, parent u is the parent of u in the dfs tree if it was in the
     * connected component and not v and None otherwise (the root of the tree is
     * v), dfstimes u is the (start,finish) times of u during the dfs if it was
     * in the connected component and None otherwise.
     *)
    val dfs1 : init:('a)
               -> onvisit:('a -> vtx -> 'a)
               -> compare:(vtx -> vtx * wt -> vtx * wt -> int)
               -> ontraversal:('a -> vtx -> vtx -> wt -> 'a)
               -> onrevisit:('a -> vtx -> vtx -> wt -> 'a)
               -> onfinish:('a -> vtx -> 'a)
               -> t
               -> vtx
               -> 'a * (vtx -> vtx option) * (vtx -> (int * int) option)


    val bfs : init:('a)
              -> onvisit:('a -> vtx -> 'a)
              -> compare:(vtx -> vtx * wt -> vtx * wt -> int)
              -> ontraversal:('a -> vtx -> vtx -> wt -> 'a)
              -> onrevisit:('a -> vtx -> vtx -> wt -> 'a)
              -> onfinish:('a -> vtx -> 'a)
              -> t
              -> 'a * (vtx -> vtx option)

    val bfs1 : init:('a)
               -> onvisit:('a -> vtx -> 'a)
               -> compare:(vtx -> vtx * wt -> vtx * wt -> int)
               -> ontraversal:('a -> vtx -> vtx -> wt -> 'a)
               -> onrevisit:('a -> vtx -> vtx -> wt -> 'a)
               -> onfinish:('a -> vtx -> 'a)
               -> t
               -> vtx
               -> 'a * (vtx -> vtx option)

  end
