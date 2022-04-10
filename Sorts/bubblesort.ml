open Core

module Bubblesort : SORT = struct

  let rec bubblesort_onepass = function
    | x::y::xs ->
      if x > y
      then let (xxs,_) = bubblesort_onepass (x::xs) in (y::xxs, true)
      else let (yxs,swap) = bubblesort_onepass (y::xs) in (x::yxs, swap)
    | xs -> (xs, false)

  let rec sort l =
    let (lpassed,swap) = bubblesort_onepass l in
    if swap then sort lpassed else lpassed

end
