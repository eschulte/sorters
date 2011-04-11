open Sys
open Printf

let rec quicksort gt = function
  | [] -> []
  | x::xs ->
      let ys, zs = List.partition (gt x) xs in
      (quicksort gt ys) @ (x :: (quicksort gt zs))

let main() =
  let as_ints = List.map int_of_string (List.tl (Array.to_list Sys.argv)) in
  let sorted = quicksort (>) as_ints in
  List.iter (fun x -> Printf.printf "%d " x) sorted;;

main();;
