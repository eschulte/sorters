open Char

let () =
  let do_c = ref true in
  let caps char =
    output_char stdout (if !do_c then uppercase char else char);
    if (compare ' ' char) = 0 then do_c := true else do_c := false in
  try
    while true do caps (input_char stdin); done;
  with
    End_of_file -> ()
