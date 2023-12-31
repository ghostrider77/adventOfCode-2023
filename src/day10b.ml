type coord = { x : int; y : int }
type direction = North | East | South | West
type position = { coord : coord; heading : direction }
type grid = { tiles : char array array; nr_rows : int; nr_columns : int }


let parse_input (lines : string list) : grid =
  match lines with
    | [] -> failwith "Empty input"
    | h :: _ ->
        let nr_rows = List.length lines in
        let nr_columns = String.length h in
        let tiles = Array.of_list @@ List.map (fun line -> Array.of_seq (String.to_seq line)) lines in
        {tiles; nr_rows; nr_columns}


let find_start_node ({tiles; _} : grid) : coord =
  let rec loop ix = function
    | [] -> failwith "Start node is not found."
    | row :: rest ->
        match Array.find_mapi (fun j c -> if c = 'S' then Some j else None) row with
          | None -> loop (ix + 1) rest
          | Some jy -> {x = ix; y = jy} in
  let coord = loop 0 (Array.to_list tiles) in
  coord


let find_next_tile ({tiles; _} : grid) ({coord = {x; y}; heading} : position) : position =
  let {x = x'; y = y'} as coord' =
    match heading with
      | North -> {x = x - 1; y = y}
      | East -> {x = x; y = y + 1}
      | South -> {x = x + 1; y = y}
      | West -> {x = x; y = y - 1} in
  let heading' =
    match (tiles.(x').(y'), heading) with
      | ('|',  South) -> South
      | ('|',  North) -> North
      | ('-',  East) -> East
      | ('-',  West) -> West
      | ('L',  South) -> East
      | ('L',  West) -> North
      | ('J',  South) -> West
      | ('J',  East) -> North
      | ('7',  North) -> West
      | ('7',  East) -> South
      | ('F', North) -> East
      | ('F', West) -> South
      | ('S', _) -> North
      | _ -> failwith "The loop ended unexpectedly."
  in {coord = coord'; heading = heading'}


let determine_start_heading ({tiles; nr_rows; nr_columns} : grid) ({x; y} : coord) : position =
  let is_on_grid {x; y} =
    0 <= x && x < nr_rows && 0 <= y && y < nr_columns in
  let is_valid_heading = function
    | North when is_on_grid {x = x - 1; y = y} ->
        let symbol = tiles.(x-1).(y) in
        symbol = '|' || symbol = '7' || symbol = 'F'
    | East when is_on_grid {x = x; y = y + 1} ->
        let symbol = tiles.(x).(y+1) in
        symbol = '-' || symbol = 'J' || symbol = '7'
    | South when is_on_grid {x = x + 1; y = y} ->
        let symbol = tiles.(x+1).(y) in
        symbol = '|' || symbol = 'L' || symbol = 'J'
    | West when is_on_grid {x = x; y = y - 1} ->
        let symbol = tiles.(x).(y-1) in
        symbol = '-' || symbol = 'L' || symbol = 'F'
    | _ -> false in

  match List.find_opt is_valid_heading [North; East; South; West] with
    | Some heading -> {coord = {x; y}; heading}
    | None -> failwith "Nowhere to go."


let traverse_loop ({tiles; _} as grid : grid) : coord list =
  let start_cell = find_start_node grid in
  let initial_position = determine_start_heading grid start_cell in
  let is_start_cell {coord = {x; y}; _} =
    tiles.(x).(y) = 'S' in
  let rec loop acc current_position =
    let {coord; _} as next_position = find_next_tile grid current_position in
    if is_start_cell next_position then List.rev acc
    else loop (coord :: acc) next_position in
  loop [start_cell] initial_position


let count_enclosed_cells (grid : grid) : int =
  let path = grid |> traverse_loop |> Array.of_list in
  let n = Array.length path in
  let process acc (ix, {x; _}) =
    if ix = 0 then acc + x * (path.(1).y - path.(n - 1).y)
    else if ix = n - 1 then acc + x * (path.(0).y - path.(n - 2).y)
    else acc + x * (path.(ix + 1).y - path.(ix - 1).y) in
  let double_area = abs @@ Array.fold_left process 0 @@ Array.mapi (fun ix item -> (ix, item)) path in
  (double_area - n + 2) / 2


let () =
  let filename = "../resources/input_10.txt" in
  let lines = In_channel.with_open_text filename In_channel.input_lines in
  let grid = parse_input lines in
  let result = count_enclosed_cells grid in
  print_int result; print_newline ()
