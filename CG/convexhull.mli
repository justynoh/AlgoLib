(* convexhull.mli
 * Defines a signature for a convex hull algorithm.
 *)

open Core

module type CONVEXHULL =
  sig

    (* This exception is to be raised when the input to p has less than 3
     * points. *)
    exception LessThanThreePoints

    type input = (float * float) list
    type output = (float * float) list

    (* ch p
     * Assumes p is an unordered of n >= 3 unique, non-collinear points in
     * the 2D plane. The output is a list containing a subset of points of P
     * which defines the convex hull, when read counterclockwise, starting from
     * the lowest-then-leftmost point of the hull. In particular, if the output
     * is [p1, p2, ... , pn], the edges forming the convex hull are p1p2, p2p3,
     * ..., pnp1.
     *)
    val ch : input -> output

  end
