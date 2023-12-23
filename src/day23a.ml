type coord = { x : int; y : int }
type slope = Up | Right | Down | Left
type tile =
  | Trail
  | Forest
  | Slope of slope
type hiking_map = { grid : tile array array; nrows : int; ncols : int }

module CoordSet = Set.Make(
  struct
    type t = coord
    let compare = Stdlib.compare
  end)

type path = { head : coord; visited : CoordSet.t }

let tile_of_char = function
  | '.' -> Trail
  | '#' -> Forest
  | '^' -> Slope Up
  | '>' -> Slope Right
  | 'v' -> Slope Down
  | '<' -> Slope Left
  | _ -> failwith "Unknown slope type."


let parse_input = function
  | [] -> failwith "Empty input."
  | (h :: _) as lines ->
      let nrows = List.length lines in
      let ncols = String.length h in
      let parse_row row =
        row |> String.to_seq |> Seq.map tile_of_char |> Array.of_seq in
      let grid = Array.of_list @@ List.map parse_row lines in
      {grid; nrows; ncols}


let get_neighbors ({grid; nrows; ncols} : hiking_map) ({x; y} : coord) : coord list =
  let is_valid {x; y} =
    0 <= x && x < nrows && 0 <= y && y < ncols && grid.(x).(y) <> Forest in
  let potential_neighbors =
    match grid.(x).(y) with
      | Slope Up -> [{x = x - 1; y}]
      | Slope Right -> [{x; y = y + 1}]
      | Slope Down -> [{x = x + 1; y}]
      | Slope Left -> [{x; y = y - 1}]
      | Trail -> [{x = x - 1; y}; {x; y = y + 1}; {x = x + 1; y}; {x; y = y - 1}]
      | Forest -> failwith "One cannot enter the forest." in
  List.filter is_valid potential_neighbors


let find_longest_path (map : hiking_map) (start : coord) (target : coord) : int =
  let grow {head; visited} =
    let neighbors = List.filter (fun n -> not (CoordSet.mem n visited)) (get_neighbors map head) in
    List.map (fun n -> {head = n; visited = CoordSet.add head visited}) neighbors in

  let rec loop acc = function
    | [] -> acc
    | paths ->
        let paths_to_target, growing_paths = List.partition (fun {head; _} -> target = head) paths in
        let longest = List.fold_left (fun acc {visited; _} -> max (CoordSet.cardinal visited) acc) 0 paths_to_target in
        let paths' = List.concat_map grow growing_paths in
        loop (max acc longest) paths' in

  loop 0 [{head = start; visited = CoordSet.empty}]


let find_longest_trail ({grid; nrows; _} as map : hiking_map) : int =
  let start_column =
    match Array.find_index (fun t -> t = Trail) grid.(0) with
      | None -> failwith "Start tile not found."
      | Some coord -> coord in
  let target_column =
    match Array.find_index (fun t -> t = Trail) grid.(nrows - 1) with
      | None -> failwith "Target tile not found."
      | Some coord -> coord in
  let start = {x = 0; y = start_column} in
  let target = {x = nrows - 1; y = target_column} in
  find_longest_path map start target


let () =
  let filename = "../resources/input_23.txt" in
  let lines = In_channel.with_open_text filename In_channel.input_lines in
  let map = parse_input lines in
  let result = find_longest_trail map in
  print_int result; print_newline ()
