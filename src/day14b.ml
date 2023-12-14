type direction = North | West | South | East
type terrain = RoundedRock | CubeRock | Empty
type coord = { x : int; y : int }

module CoordSet = Set.Make(
  struct
    type t = coord
    let compare { x = x1; y = y1 } { x = x2; y = y2 } = compare (x1, y1) (x2, y2)
  end)

type platform = { cubeRocks : CoordSet.t; roundedRocks : CoordSet.t; nrows : int; ncols : int }


let terrain_of_char = function
  | 'O' -> RoundedRock
  | '#' -> CubeRock
  | '.' -> Empty
  | _ -> failwith "Unknown terrain type."


let parse_platform = function
  | [] -> failwith "Empty grid"
  | (h :: _) as lines ->
      let nrows = List.length lines in
      let ncols = String.length h in
      let parse_row x row =
        row |> String.to_seqi |> Seq.map (fun (y, chr) -> ({x; y}, terrain_of_char chr)) |> List.of_seq in
      let select_coords_by_type coords terrain_type =
        List.filter_map (fun (coord, item) -> if item = terrain_type then Some coord else None) coords in
      let coordinates = List.(concat @@ mapi parse_row lines) in
      let cubeRocks = select_coords_by_type coordinates CubeRock in
      let roundedRocks = select_coords_by_type coordinates RoundedRock in
      { cubeRocks = CoordSet.of_list cubeRocks; roundedRocks = CoordSet.of_list roundedRocks; nrows; ncols }


let tilt_column (cubeRocks : coord list) (roundedRocks : coord list) : coord list =
  let rec loop acc ix rs cs =
    match (rs, cs) with
      | ([], _) -> List.rev acc
      | ({y; _} :: rss, []) -> loop ({x = ix; y} :: acc) (ix + 1) rss cs
      | ({x = rx; y} :: rss, {x = cx; _} :: css) ->
          if rx < cx then loop ({x = ix; y} :: acc) (ix + 1) rss cs
          else loop acc (cx + 1) rs css in
  loop [] 0 roundedRocks cubeRocks


let select_transformed_column {nrows; ncols; _} rs k = function
  | North -> CoordSet.(rs |> filter (fun {y; _} -> y = k) |> elements)
  | West -> CoordSet.(rs |> filter_map (fun {x; y} -> if x = k then Some {x = y; y = x} else None) |> elements)
  | South -> CoordSet.(rs |> filter_map (fun {x; y} -> if y = k then Some {x = nrows-x-1; y} else None) |> elements)
  | East -> CoordSet.(rs |> filter_map (fun {x; y} -> if x = k then Some {x = ncols-y-1; y = x} else None) |> elements)


let transform_back {nrows; ncols; _} rs = function
  | North -> CoordSet.of_list rs
  | West -> CoordSet.of_list @@ List.map (fun {x; y} -> {x = y; y = x}) rs
  | South -> CoordSet.of_list @@ List.map (fun {x; y} -> {x = nrows - x - 1; y}) rs
  | East -> CoordSet.of_list @@ List.map (fun {x; y} -> {x = y; y = ncols - x - 1}) rs


let tilt ({cubeRocks; roundedRocks; ncols; nrows} as plt : platform) (direction : direction) : platform =
  let tilt_column j =
    let cubes = select_transformed_column plt cubeRocks j direction in
    let rocks = select_transformed_column plt roundedRocks j direction in
    let rocks' = tilt_column cubes rocks in
    transform_back plt rocks' direction in

  let k =
    match direction with
      | North | South -> ncols
      | West | East -> nrows in
  let roundedRocks' =
    Seq.fold_left (fun acc k -> CoordSet.union acc (tilt_column k)) CoordSet.empty (Seq.init k Fun.id) in
  { plt with roundedRocks = roundedRocks' }


let calc_north_tilting_score ({roundedRocks; nrows; _} : platform) : int =
  CoordSet.fold (fun {x; _} acc -> nrows - x + acc) roundedRocks 0


let tilt_in_cycle (platform : platform) : platform =
  List.fold_left tilt platform [North; West; South; East]


let find_tilting_cycle_period (platform : platform) : platform * int * int =
  let rec loop acc current_platform =
    let {roundedRocks; _} as next_platform = tilt_in_cycle current_platform in
    match List.find_index (fun rs -> rs = roundedRocks) acc with
      | Some ix -> (current_platform, List.length acc - ix - 1, ix + 1)
      | None -> loop (roundedRocks :: acc) next_platform in
  loop [platform.roundedRocks] platform


let calc_total_load (platform : platform) (n : int) : int =
  let platform', ix, period = find_tilting_cycle_period platform in
  let k = (n - ix) mod period + 1 in
  let result = Seq.fold_left (fun acc _ -> tilt_in_cycle acc) platform' (Seq.init k Fun.id) in
  calc_north_tilting_score result


let () =
  let filename = "../resources/input_14.txt" in
  let lines = In_channel.with_open_text filename In_channel.input_lines in
  let n = 1000000000 in
  let platform = parse_platform lines in
  let result = calc_total_load platform n in
  print_int result; print_newline ()