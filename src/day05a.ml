type category = Seed | Soil | Fertilizer | Water | Light | Temperature | Humidity | Location
type conversion_map = { destination_start : int; source_start : int; range_length : int }
type category_mapping = { category_from : category; category_to : category; mappings : conversion_map list }


let category_of_string = function
  | "seed" -> Seed
  | "soil" -> Soil
  | "fertilizer" -> Fertilizer
  | "water" -> Water
  | "light" -> Light
  | "temperature" -> Temperature
  | "humidity" -> Humidity
  | "location" -> Location
  | s -> failwith ("Unknown category " ^ s)


let parse_input (lines : string list) : int list * category_mapping list =
  let open Batteries in
  let extract_seeds line =
    line |> Str.split (Str.regexp "[ \t]+") |> List.tl |> List.map int_of_string in
  let read_conversion_map line =
    Scanf.sscanf line "%d %d %d" (fun d s r -> {destination_start = d; source_start = s; range_length = r}) in
  let rec loop acc = function
    | [] -> List.rev acc
    | _ :: ls ->
      let (block, rest) = List.span (fun l -> not (String.is_empty l)) ls in
      let (category1, category2) =
        Scanf.sscanf (List.hd block) "%s@-to-%s map" (fun c1 c2 -> category_of_string c1, category_of_string c2) in
      let mappings = block |> List.tl |> List.map read_conversion_map in
      let category_mapping = {category_from = category1; category_to = category2; mappings} in
      loop (category_mapping :: acc) rest in
  let seeds = extract_seeds (List.hd lines) in
  let category_mappings = loop [] (List.tl lines) in
  (seeds, category_mappings)


let calc_location (seed : int) (category_mappings : category_mapping list) : int =
  let apply_mappings (n : int) {mappings; _} : int =
    let rec loop = function
      | [] -> n
      | {destination_start; source_start; range_length} :: rest ->
          if n >= source_start && n < source_start + range_length then n + destination_start - source_start
          else loop rest in
    loop mappings in

  List.fold_left apply_mappings seed category_mappings


let calc_lowest_location_number (seeds : int list) (category_mappings : category_mapping list) : int =
  List.fold_left (fun acc seed -> min acc (calc_location seed category_mappings)) max_int seeds


let () =
  let filename = "../resources/input_05.txt" in
  let lines = In_channel.with_open_text filename In_channel.input_lines in
  let (seeds, category_mappings) = parse_input lines in
  let result = calc_lowest_location_number seeds category_mappings in
  print_int result; print_newline ()
