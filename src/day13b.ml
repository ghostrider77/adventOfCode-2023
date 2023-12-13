type terrain = Ash | Rock
type pattern = { grid : terrain array array; nrows : int; ncols : int }
type reflection_type = Horizontal | Vertical
type reflection = { reflection_type : reflection_type; ix : int }

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
        { grid = Array.of_list (List.map Array.of_list ps); nrows; ncols } in

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


let horizontally_mirrored ({grid; nrows; _} : pattern) : int list =
  let is_mirrored_at ix =
    let rec loop ix1 ix2 =
      if ix1 < 0 || ix2 >= nrows then true
      else
        let row1 = grid.(ix1) in
        let row2 = grid.(ix2) in
        if row1 = row2 then loop (ix1 - 1) (ix2 + 1)
        else false in
    loop ix (ix + 1) in

  List.filter is_mirrored_at @@ List.init (nrows - 1) Fun.id


let vertically_mirrored ({grid; ncols; _} : pattern) : int list =
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

  List.filter is_mirrored_at @@ List.init (ncols - 1) Fun.id


let get_reflections (pattern : pattern) : reflection list =
  let ixs = List.map (fun ix -> { reflection_type = Horizontal; ix }) @@ horizontally_mirrored pattern in
  let jys = List.map (fun ix -> { reflection_type = Vertical; ix }) @@ vertically_mirrored pattern in
  ixs @ jys


let find_modified_reflection ({grid; nrows; ncols} as pattern : pattern) : reflection =
  let initial_reflection =
    match get_reflections pattern with
      | [r] -> r
      | _ -> failwith "More than one reflection was found" in
  let rec loop ix jy =
    if ix = nrows then failwith "No smudge was found"
    else if jy = ncols then loop (ix + 1) 0
    else
      let grid' = Array.(map copy grid) in
      let x = grid'.(ix).(jy) in
      grid'.(ix).(jy) <- if x = Ash then Rock else Ash;
      let rs = get_reflections { grid = grid'; nrows; ncols } in
      match List.find_opt (fun r -> r <> initial_reflection) rs with
        | Some r -> r
        | _ -> loop ix (jy + 1) in
  loop 0 0


let summarize_pattern_nodes (patterns : pattern list) : int =
  let process acc pattern =
    let {reflection_type; ix} = find_modified_reflection pattern in
    match reflection_type with
      | Horizontal -> acc + 100 * (ix + 1)
      | Vertical -> acc + (ix + 1) in
  List.fold_left process 0 patterns


let () =
  let filename = "../resources/input_13.txt" in
  let lines = In_channel.with_open_text filename In_channel.input_lines in
  let patterns = parse_patterns lines in
  let result = summarize_pattern_nodes patterns in
  print_int result; print_newline ()
