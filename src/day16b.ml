type cell = Empty | LeftMirror | RightMirror | VerticalSplitter | HorizontalSplitter
type contraption = { grid : cell array array; nrows : int; ncols : int }

type direction = Up | Down | Left | Right
type tile = { x : int; y : int }
type beam = { position : tile; heading : direction }

module TileSet = Set.Make(
  struct
    type t = tile
    let compare = Stdlib.compare
  end)

module BeamSet = Set.Make(
  struct
    type t = beam
    let compare = Stdlib.compare
  end)


let cell_of_char = function
  | '.' -> Empty
  | '\\' -> LeftMirror
  | '/' -> RightMirror
  | '|' -> VerticalSplitter
  | '-' -> HorizontalSplitter
  | _ -> failwith "Unknown cell type."


let parse_contraption = function
  | [] -> failwith "Empty input"
  | (h :: _) as lines ->
      let nrows = List.length lines in
      let ncols = String.length h in
      let parse_row row =
        row |> String.to_seq |> Seq.map cell_of_char |> Array.of_seq in
      let grid = Array.of_list @@ List.map parse_row lines in
      {grid; nrows; ncols}


let change_heading (cell : cell) (previous_heading : direction) : direction list =
  match (cell, previous_heading) with
    | (Empty, _) -> [previous_heading]
    | (LeftMirror, Down) -> [Right]
    | (LeftMirror, Left) -> [Up]
    | (LeftMirror, Up) -> [Left]
    | (LeftMirror, Right) -> [Down]
    | (RightMirror, Down) -> [Left]
    | (RightMirror, Left) -> [Down]
    | (RightMirror, Up) -> [Right]
    | (RightMirror, Right) -> [Up]
    | (VerticalSplitter, Up) | (VerticalSplitter, Down) -> [previous_heading]
    | (VerticalSplitter, Left) | (VerticalSplitter, Right) -> [Up; Down]
    | (HorizontalSplitter, Left) | (HorizontalSplitter, Right) -> [previous_heading]
    | (HorizontalSplitter, Up) | (HorizontalSplitter, Down) -> [Left; Right]


let move ({grid; nrows; ncols} : contraption) ({position = {x; y}; heading} : beam) : BeamSet.t =
  let is_position_valid {x; y} =
    0 <= x && x < nrows && 0 <= y && y < ncols in
  let {x = x'; y = y'} as position' =
    match heading with
      | Up -> {x = x - 1; y = y}
      | Down -> {x = x + 1; y = y}
      | Left -> {x = x; y = y - 1}
      | Right -> {x = x; y = y + 1} in
  if is_position_valid position' then
    let next_cell = grid.(x').(y') in
    let headings' = change_heading next_cell heading in
    BeamSet.of_list (List.map (fun h -> {position = position'; heading = h}) headings')
  else BeamSet.empty


let reflect_beams (contraption : contraption) (start_beam : beam) : int =
  let rec loop visited_beams beams =
    if BeamSet.is_empty beams then
      BeamSet.fold (fun {position; _} acc -> TileSet.add position acc) visited_beams TileSet.empty
    else
      let beams' = BeamSet.fold (fun beam acc -> BeamSet.union acc (move contraption beam)) beams BeamSet.empty in
      let next_beams = BeamSet.filter (fun beam -> not (BeamSet.mem beam visited_beams)) beams' in
      loop (BeamSet.union visited_beams next_beams) next_beams in

  let tiles = loop BeamSet.empty (BeamSet.singleton start_beam) in
  TileSet.cardinal tiles


let find_maximal_energy ({nrows; ncols; _} as contraption : contraption) : int =
  let top = Seq.map (fun y -> {position = {x = -1; y = y}; heading = Down}) (Seq.init ncols Fun.id) in
  let left = Seq.map (fun x -> {position = {x = x; y = -1}; heading = Right}) (Seq.init nrows Fun.id) in
  let bottom = Seq.map (fun y -> {position = {x = nrows; y = y}; heading = Up}) (Seq.init ncols Fun.id) in
  let right = Seq.map (fun x -> {position = {x = x; y = ncols}; heading = Left}) (Seq.init nrows Fun.id) in
  let initial_beams = Seq.concat_map Fun.id (List.to_seq [top; left; bottom; right]) in
  Seq.fold_left (fun acc start_beam -> max acc (reflect_beams contraption start_beam)) 0 initial_beams


let () =
  let filename = "../resources/input_16.txt" in
  let lines = In_channel.with_open_text filename In_channel.input_lines in
  let contraption = parse_contraption lines in
  let result = find_maximal_energy contraption in
  print_int result; print_newline ()
