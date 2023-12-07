type card = { id : int; winning_numbers : int list; own_numbers : int list }


let convert_to_intlist (line : string) : int list =
  List.map int_of_string Str.(line |> split (regexp "[ \t]+"))


let read_lines (filename : string) : string list =
  In_channel.with_open_text filename (In_channel.input_lines)


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


let calc_total_number_of_cards (cards : card list) : int =
  let nr_cards = List.length cards in
  let card_counts = Array.make nr_cards 1 in
  let calc_number_of_matches {winning_numbers; own_numbers; _} =
    List.fold_left (fun acc n -> if List.mem n winning_numbers then acc + 1 else acc) 0 own_numbers in
  let process_card ({id; _} as card) =
    let nr_copies = card_counts.(id - 1) in
    let nr_matches = calc_number_of_matches card in
    let ids = Seq.filter (fun card_id -> card_id <= nr_cards) @@ Seq.init nr_matches (fun k -> id + k + 1) in
    Seq.iter (fun k -> card_counts.(k - 1) <- card_counts.(k - 1) + nr_copies) ids; in
  List.iter process_card cards;
  Array.fold_left (+) 0 card_counts


let () =
  let lines = read_lines "../resources/input_04.txt" in
  let cards = parse_input lines in
  let result = calc_total_number_of_cards cards in
  print_int result; print_newline ()
