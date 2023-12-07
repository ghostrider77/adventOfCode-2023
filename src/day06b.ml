type race = { time : int; record : int }

let read_lines (filename : string) : string list =
  In_channel.with_open_text filename (In_channel.input_lines)


let parse_input (lines : string list) : race =
  let read_tail_as_number line =
    line |> Str.split (Str.regexp "[ \t]+") |> List.tl |> String.concat "" |> int_of_string in
  match lines with
    | [a; b] -> { time = read_tail_as_number a; record = read_tail_as_number b }
    | _ -> failwith "Malformed input."


let calc_record_breaking_product ({time; record} : race) : int =
  let discriminant = float (time * time - 4 * record) in
  if discriminant <= 0.0 then 0
  else
    let d = sqrt discriminant in
    let x1 = (float(time) -. d) /. 2.0 in
    let x2 = (float(time) +. d) /. 2.0 in
    let n1 = int_of_float (if x1 = ceil x1 then x1 +. 1.0 else ceil x1) in
    let n2 = int_of_float (if x2 = floor x2 then x2 -. 1.0 else floor x2) in
    max 0 (n2 - n1 + 1)


let () =
  let lines = read_lines "../resources/input_06.txt" in
  let race = parse_input lines in
  let result = calc_record_breaking_product race in
  print_int result; print_newline ()
