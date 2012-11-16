open Sys
open Printf

let rec bsort s =
  let rec _bsort = function
    | x :: x2 :: xs when x > x2 ->
      x2 :: _bsort (x :: xs)
    | x :: x2 :: xs ->
      x :: _bsort (x2 :: xs)
    | s -> s
  in
  let t = _bsort s in
  if t = s then t
  else bsort t

let main() =
  let as_ints = List.map int_of_string (List.tl (Array.to_list Sys.argv)) in
  let sorted = bsort as_ints in
  List.iter (fun x -> Printf.printf "%d " x) sorted;;

main();;
