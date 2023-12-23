type coord = { x : int; y : int }
type tile = Trail | Forest
type hiking_map = { grid : tile array array; nrows : int; ncols : int }
type edge = { coord : coord; weight : int }

module CoordSet = Set.Make(
  struct
    type t = coord
    let compare = Stdlib.compare
  end)

module CoordMap = Map.Make(
  struct
    type t = coord
    let compare = Stdlib.compare
  end)

type path = { head : coord; visited : CoordSet.t; length : int }


let tile_of_char = function
  | '.' | '^' | '>' | 'v' | '<' -> Trail
  | '#' -> Forest
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
      | Trail -> [{x = x - 1; y}; {x; y = y + 1}; {x = x + 1; y}; {x; y = y - 1}]
      | Forest -> [] in
  List.filter is_valid potential_neighbors


let find_junctions ({nrows; ncols; _} as map : hiking_map) : CoordSet.t =
  let coords = Seq.flat_map (fun x -> Seq.map (fun y -> {x; y}) (Seq.init ncols Fun.id)) (Seq.init nrows Fun.id) in
  let is_junction coord =
    let neighbors = get_neighbors map coord in
    List.length neighbors >= 3 in

  let junctions = Seq.filter is_junction coords in
  CoordSet.of_seq junctions


let create_junction_graph (map : hiking_map) (start : coord) (target : coord) : edge list CoordMap.t =
  let junctions = map |> find_junctions |> CoordSet.add start |> CoordSet.add target in
  let find_edges coord =
    let rec extend {head; visited; length} =
      match List.filter (fun n -> not (CoordSet.mem n visited)) (get_neighbors map head) with
        | [neighbor] ->
            if CoordSet.mem neighbor junctions then Some {coord = neighbor; weight = length + 1}
            else extend {head = neighbor; visited = CoordSet.add head visited; length = length + 1}
        | _ -> None in
    let neighbors = get_neighbors map coord in
    List.filter_map (fun n -> extend {head = n; visited = CoordSet.singleton coord; length = 1}) neighbors in

  CoordSet.fold (fun coord acc -> let ns = find_edges coord in CoordMap.add coord ns acc) junctions CoordMap.empty


let calc_length_of_longest_trail (graph : edge list CoordMap.t) (start : coord) (target : coord) : int =
  let grow {head; visited; length} =
    let neighbors = List.filter (fun {coord; _} -> not (CoordSet.mem coord visited)) (CoordMap.find head graph) in
    List.map (fun {coord; weight} -> {head = coord; visited = CoordSet.add head visited; length = length + weight})
      neighbors in

  let rec loop acc = function
    | [] -> acc
    | paths ->
        let paths_to_target, growing_paths = List.partition (fun {head; _} -> target = head) paths in
        let longest = List.fold_left (fun acc {length; _} -> max length acc) 0 paths_to_target in
        let paths' = List.concat_map grow growing_paths in
        loop (max acc longest) paths' in

  loop 0 [{head = start; visited = CoordSet.empty; length = 0}]


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
  let graph = create_junction_graph map start target in
  calc_length_of_longest_trail graph start target


let () =
  let filename = "../resources/input_23.txt" in
  let lines = In_channel.with_open_text filename In_channel.input_lines in
  let map = parse_input lines in
  let result = find_longest_trail map in
  print_int result; print_newline ()
