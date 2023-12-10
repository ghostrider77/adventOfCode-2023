type category = Seed | Soil | Fertilizer | Water | Light | Temperature | Humidity | Location
type conversion_map = { destination_start : int; source_start : int; range_length : int }
type category_mapping = { category_from : category; category_to : category; mappings : conversion_map list }
type seed_range = { left : int; right : int }

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


let parse_input (lines : string list) : seed_range list * category_mapping list =
  let open Batteries in
  let extract_seeds line =
    let seed_values = line |> Str.split (Str.regexp "[ \t]+") |> List.tl |> List.map int_of_string in
    let rec loop acc = function
      | [] -> List.rev acc
      | left :: length :: rest -> loop ({ left; right = left + length - 1 } :: acc) rest
      | _ -> failwith "Malformed input." in
    loop [] seed_values in
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


let apply_category_mappings (initial_seeds : seed_range list) ({mappings; _} : category_mapping) : seed_range list =
  let seeds = Queue.of_seq (List.to_seq initial_seeds) in
  let rec apply_mappings acc ms =
    match Queue.take_opt seeds with
      | None -> List.rev acc
      | Some ({left; right} as seed) ->
        let rec loop = function
          | [] -> seed
          | {destination_start; source_start; range_length} :: mss ->
              if right <= source_start || source_start + range_length <= left then loop mss
              else
                let a = max left source_start in
                let b = min right (source_start + range_length) in
                if left < a then
                  Queue.add {left; right = a} seeds;
                if b < right then
                  Queue.add {left = b; right} seeds;
                let c = destination_start - source_start in
                {left = a + c; right = b + c} in
        let seed' = loop ms in
      apply_mappings (seed' :: acc) ms in
  apply_mappings [] mappings


let calc_lowest_location_number (seeds : seed_range list) (category_mappings : category_mapping list) : int =
  List.(category_mappings |> fold_left (fun seeds' cat_maps -> apply_category_mappings seeds' cat_maps) seeds
                          |> fold_left (fun acc {left; _} -> min acc left) max_int)


let () =
  let filename = "../resources/input_05.txt" in
  let lines = In_channel.with_open_text filename In_channel.input_lines in
  let (seeds, category_mappings) = parse_input lines in
  let result = calc_lowest_location_number seeds category_mappings in
  print_int result; print_newline ()
