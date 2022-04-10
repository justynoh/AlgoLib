(* tester.mli
 * Implements the signature for the test suite.
 *)

open Core

module type TESTER =
  sig

    exception TestFailed of int

    type input
    type output

    val testdata : (input * output) array

    val outputequal : output -> output -> bool

    val test1 : int -> unit

    val test : unit -> unit

  end
