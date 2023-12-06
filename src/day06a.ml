type race = { time : int; record : int }


let read_lines (filename : string) : string list =
  let channel = open_in filename in
  let rec loop acc =
    try
      let line = input_line channel in
      loop (line :: acc)
    with End_of_file -> List.rev acc in
  let lines = loop [] in
  close_in channel;
  lines


let parse_input (lines : string list) : race list =
  let convert_tail_to_intlist line =
    line |> Str.split (Str.regexp "[ \t]+") |> List.tl |> List.map int_of_string in
  match lines with
    | [a; b] ->
      let ts = convert_tail_to_intlist a in
      let rs = convert_tail_to_intlist b in
      List.rev @@ List.fold_left2 (fun acc t r -> {time = t; record = r} :: acc) [] ts rs
    | _ -> failwith "Malformed input."


let calc_record_breaking_product (races : race list) : int =
  let calc_nr_record_breaking {time; record} =
    let det = sqrt @@ float @@ time * time - 4 * record in
    if det <= 0.0 then 0
    else
      let x1 = (float(time) -. det) /. 2.0 in
      let x2 = (float(time) +. det) /. 2.0 in
      let n1 = int_of_float (if x1 = ceil x1 then x1 +. 1.0 else ceil x1) in
      let n2 = int_of_float (if x2 = floor x2 then x2 -. 1.0 else floor x2) in
      max 0 (n2 - n1 + 1) in

  List.fold_left (fun acc race -> acc * calc_nr_record_breaking race) 1 races


let () =
  let lines = read_lines "../resources/input_06.txt" in
  let races = parse_input lines in
  let result = calc_record_breaking_product races in
  print_int result; print_newline ()
