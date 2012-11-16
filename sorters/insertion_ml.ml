open Sys
open Printf

let rec insert x = function
  [] -> [x]
| y :: ys ->
   if x <= y then x :: y :: ys
   else y :: insert x ys
;;
let insertion_sort lst = List.fold_right insert lst [];;

let main() =
  let as_ints = List.map int_of_string (List.tl (Array.to_list Sys.argv)) in
  let sorted = insertion_sort as_ints in
  List.iter (fun x -> Printf.printf "%d " x) sorted;;

main();;
