let read_lines (filename : string) : string list =
  let channel = open_in filename in
  let rec loop (acc : string list) : string list =
    try
      let line = input_line channel in
      loop (line :: acc)
    with End_of_file -> List.rev acc in
  let lines = loop [] in
  close_in channel;
  lines


let collect_digits (s: string) : char list =
  let is_digit = function '0' .. '9' -> true | _ -> false in
    List.rev @@ String.fold_left (fun acc char -> if is_digit char then char :: acc else acc) [] s


let calibration_sum (calibration_strings : string list) : int =
  let get_calibration_value (s : string) : int =
    match collect_digits s with
      | [] -> failwith "No digit was found."
      | [d] -> int_of_string (Printf.sprintf "%c%c" d d)
      | hd :: tl -> int_of_string (Printf.sprintf "%c%c" hd (List.hd (List.rev tl))) in
  List.fold_left (fun acc s -> acc + get_calibration_value s) 0 calibration_strings


let () =
  let lines = read_lines "../resources/input_01.txt" in
  let result = calibration_sum lines in
  print_int result; print_newline ()
