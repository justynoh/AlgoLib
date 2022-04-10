(* convexhull_generatetest.ml
 * Generates a valid test for the convex hull problem based on the global
 * settings provided..
 *)

open Core

(* Global settings to generate a testcase. *)
let output = [| (-6.,-4.);(-2.,-3.5);(1.,-2.);(3.,-0.5);(7.,3.);(5.,4.);(3.,4.);(-2.,3.75);(-4.,3.5);(-6.,3.);(-8.,2.);(-10.,0.);(-10.,-1.);(-9.,-3.);(-8.,-3.5) |]
let inputsize = 250


(* leftside (p,q) r
 * Returns true if r lies to the left of the directed line defined by (p,q).
 *)
let leftside (p,q) r =
  let (px,py) = p in
  let (qx,qy) = q in
  let (rx,ry) = r in
  Float.((qx - px) * (ry - py) - (qy - py) * (rx - px) > 0.)

(* collinear p q r
 * Returns true if p, q and r are collinear.
 *)
let collinear p q r =
  let (px,py) = p in
  let (qx,qy) = q in
  let (rx,ry) = r in
  Float.((qx - px) * (ry - py) - (qy - py) * (rx - px) = 0.)

let array_to_string ~f = Array.fold ~init:"" ~f:(fun acc e -> acc ^ (f e) ^ " ; \n")

let floatpair_to_string (x,y) = "(" ^ Float.to_string x ^ "," ^ Float.to_string y ^ ")"

let () =
  (* First go through the output and add it to the input array. *)
  let outputsize = Array.length output in
  let input = Array.init inputsize ~f:(fun i -> if i < outputsize then Some output.(i) else None) in
  let (xs,ys) = Array.unzip output in
  let (Some x1, Some x2) = (Array.min_elt ~compare:Float.compare xs, Array.max_elt ~compare:Float.compare xs) in
  let (Some y1, Some y2) = (Array.min_elt ~compare:Float.compare ys, Array.max_elt ~compare:Float.compare ys) in
  for i = outputsize to inputsize - 1 do
    while Option.is_none input.(i) do
      (* Generate an element in the bounding box. *)
      let (px,py) = (Random.float_range x1 x2, Random.float_range y1 y2) in
      let inhull = Array.foldi output ~init:true ~f:(fun i acc p1 ->
        let p2 = if i = outputsize - 1 then output.(0) else output.(i+1) in
        if leftside (p1,p2) (px,py) then acc else false
        ) in
      let iscol = Array.foldi input ~init:false ~f:(fun j curr p1 ->
          Array.foldi input ~init:curr ~f:(fun k curr p2 ->
            if j < k && k < i && collinear (Option.value_exn p1) (Option.value_exn p2) (px,py) then true else curr
          )
        ) in
      if inhull && not iscol then input.(i) <- Some (px,py)
    done
  done;
  Array.permute input;
  print_endline (array_to_string (Array.map input ~f:(fun e -> Option.value_exn e)) ~f:floatpair_to_string)
