type terrain = RoundedRock | CubeRock | Empty
type grid = terrain array array


let terrain_of_char = function
  | 'O' -> RoundedRock
  | '#' -> CubeRock
  | '.' -> Empty
  | _ -> failwith "Unknown terrain type."


let parse_grid (lines : string list) : grid =
  lines
    |> List.map (fun row -> row |> String.to_seq |> Seq.map terrain_of_char |> Array.of_seq)
    |> Array.of_list


let calc_tilting_score (arr : terrain array) : int =
  let n = Array.length arr in
  let xs = Array.mapi (fun ix x -> (n - ix, x)) arr in
  let process (acc, (cube_ix, cnt)) (ix, item) =
    match item with
      | Empty -> (acc, (cube_ix, cnt))
      | RoundedRock -> (acc, (cube_ix, cnt + 1))
      | CubeRock -> (((cube_ix, cnt) :: acc), (ix, 0)) in
  let res, (ix, cnt) = Array.fold_left process ([], (n + 1, 0)) xs in
  let counts = if cnt = 0 then res else (ix, cnt) :: res in
  List.fold_left (fun acc (ix, cnt) -> List.fold_left (+) acc (List.init cnt (fun k -> ix - k - 1))) 0 counts


let calc_total_tilting_score (grid : grid) : int =
  let ncols = Array.length grid.(0) in
  let get_column j = Array.map (fun row -> row.(j)) grid in
  Seq.fold_left (fun acc j -> acc + calc_tilting_score (get_column j)) 0 (Seq.init ncols Fun.id)


let () =
  let filename = "../resources/input_14.txt" in
  let lines = In_channel.with_open_text filename In_channel.input_lines in
  let grid = parse_grid lines in
  let result = calc_total_tilting_score grid in
  print_int result; print_newline ()
