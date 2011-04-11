open Sys
open Printf

let rec split_at n xs =
  match n, xs with
      0, xs ->
        [], xs
    | _, [] ->
        failwith "index too large"
    | n, x::xs when n > 0 ->
        let xs', xs'' = split_at (pred n) xs in
          x::xs', xs''
    | _, _ ->
        invalid_arg "negative argument"
 
let rec merge_sort cmp = function
    [] -> []
  | [x] -> [x]
  | xs ->
      let xs, ys = split_at (List.length xs / 2) xs in
        List.merge cmp (merge_sort cmp xs) (merge_sort cmp ys)

let main() =
  let as_ints = List.map int_of_string (List.tl (Array.to_list Sys.argv)) in
  let sorted = merge_sort compare as_ints in
  List.iter (fun x -> Printf.printf "%d " x) sorted;;

main();;
