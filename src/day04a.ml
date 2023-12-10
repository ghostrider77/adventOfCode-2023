type card = { id : int; winning_numbers : int list; own_numbers : int list }


let convert_to_intlist (line : string) : int list =
  List.map int_of_string Str.(line |> split (regexp "[ \t]+"))


let parse_input (lines : string list) : card list =
  let parse line =
    match String.split_on_char ':' line with
      | [c; rest] ->
        let id = Scanf.sscanf c "Card %d" (fun d -> d) in
        (match String.split_on_char '|' rest with
          | [win; own] -> { id; winning_numbers = convert_to_intlist win; own_numbers = convert_to_intlist own }
          | _ -> failwith "Malformed input.")
      | _ -> failwith "Malformed input." in

  List.map parse lines


let calc_total_points (cards : card list) : int =
  let evaluate {winning_numbers; own_numbers; _} =
    let check_number acc n =
      if List.mem n winning_numbers then if acc = 0 then 1 else 2 * acc
      else acc in
    List.fold_left check_number 0 own_numbers in

  List.fold_left (fun acc card -> acc + evaluate card) 0 cards


let () =
  let filename = "../resources/input_04.txt" in
  let lines = In_channel.with_open_text filename In_channel.input_lines in
  let cards = parse_input lines in
  let result = calc_total_points cards in
  print_int result; print_newline ()
