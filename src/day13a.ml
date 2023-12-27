type terrain = Ash | Rock
type pattern = { grid : terrain array array; nrows : int; ncols : int }

let terrain_of_char = function
  | '.' -> Ash
  | '#' -> Rock
  | _ -> failwith "Unknown terrain type."


let parse_patterns (lines : string list) : pattern list =
  let create_pattern = function
    | [] -> failwith "Empty pattern"
    | (h :: _) as ps ->
        let nrows = List.length ps in
        let ncols = List.length h in
        {grid = Array.of_list (List.map Array.of_list ps); nrows; ncols} in

  let rec loop acc current_pattern = function
    | [] ->
        let ps = List.rev current_pattern in
        List.rev (ps :: acc)
    | l :: ls ->
        if l = "" then
          loop ((List.rev current_pattern) :: acc) [] ls
        else
          let row = l |> String.to_seq |> Seq.map terrain_of_char |> List.of_seq in
          loop acc (row :: current_pattern) ls in

  let ts = loop [] [] lines in
  List.map create_pattern ts


let horizontally_mirrored ({grid; nrows; _} : pattern) : int option =
  let is_mirrored_at ix =
    let rec loop ix1 ix2 =
      if ix1 < 0 || ix2 >= nrows then true
      else
        let row1 = grid.(ix1) in
        let row2 = grid.(ix2) in
        if row1 = row2 then loop (ix1 - 1) (ix2 + 1)
        else false in
    loop ix (ix + 1) in

  List.find_opt is_mirrored_at @@ List.init (nrows - 1) Fun.id


let vertically_mirrored ({grid; ncols; _} : pattern) : int option =
  let get_column j = Array.map (fun r -> r.(j)) grid in
  let is_mirrored_at jy =
    let rec loop jy1 jy2 =
      if jy1 < 0 || jy2 >= ncols then true
      else
        let col1 = get_column jy1 in
        let col2 = get_column jy2 in
        if col1 = col2 then loop (jy1 - 1) (jy2 + 1)
        else false in
    loop jy (jy + 1) in

  List.find_opt is_mirrored_at @@ List.init (ncols - 1) Fun.id


let summarize_pattern_nodes (patterns : pattern list) : int =
  let sum_valid_indices ns =
    ns
      |> List.map (fun n -> Option.to_list (Option.map (fun o -> o + 1) n))
      |> List.flatten
      |> List.fold_left (+) 0 in

  let ixs = List.map horizontally_mirrored patterns in
  let jys = List.map vertically_mirrored patterns in
  100 * sum_valid_indices ixs + sum_valid_indices jys


let () =
  let filename = "../resources/input_13.txt" in
  let lines = In_channel.with_open_text filename In_channel.input_lines in
  let patterns = parse_patterns lines in
  let result = summarize_pattern_nodes patterns in
  print_int result; print_newline ()
