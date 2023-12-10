let number_strings = ["one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine"; "ten"]
let number_code = List.mapi (fun ix n -> (n, ix + 1)) number_strings


let collect_digits (line : string) : int list =
  let find_prefix s = List.find_opt (fun number -> String.starts_with ~prefix:number s) number_strings in
  let is_digit = function
    | '0' .. '9' -> true
    | _ -> false in
  let length = String.length line in
  let rec loop acc k =
    if k = length then List.rev acc
    else if is_digit line.[k] then loop ((int_of_string @@ String.make 1 line.[k]) :: acc) (k + 1)
    else
      let rest = String.sub line k (length - k) in
      match find_prefix rest with
        | None -> loop acc (k + 1)
        | Some number -> loop ((List.assoc number number_code) :: acc) (k + 1) in
  loop [] 0


let calibration_sum (calibration_strings : string list) : int =
  let get_calibration_value s =
    match collect_digits s with
      | [] -> failwith "No digit was found."
      | [d] -> int_of_string (Printf.sprintf "%d%d" d d)
      | hd :: tl -> int_of_string (Printf.sprintf "%d%d" hd (List.hd (List.rev tl))) in
  List.fold_left (fun acc s -> acc + get_calibration_value s) 0 calibration_strings


let () =
  let filename = "../resources/input_01.txt" in
  let lines = In_channel.with_open_text filename In_channel.input_lines in
  let result = calibration_sum lines in
  print_int result; print_newline ()
