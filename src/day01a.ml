let collect_digits (str : string) : char list =
  let is_digit = function
    | '0' .. '9' -> true
    | _ -> false in
  List.rev @@ String.fold_left (fun acc chr -> if is_digit chr then chr :: acc else acc) [] str


let calibration_sum (calibration_strings : string list) : int =
  let get_calibration_value s =
    match collect_digits s with
      | [] -> failwith "No digit was found."
      | [d] -> int_of_string (Printf.sprintf "%c%c" d d)
      | hd :: tl -> int_of_string (Printf.sprintf "%c%c" hd (List.hd (List.rev tl))) in
  List.fold_left (fun acc s -> acc + get_calibration_value s) 0 calibration_strings


let () =
  let filename = "../resources/input_01.txt" in
  let lines = In_channel.with_open_text filename In_channel.input_lines in
  let result = calibration_sum lines in
  print_int result; print_newline ()
