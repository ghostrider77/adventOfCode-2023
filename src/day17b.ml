open Batteries

type coord = { x : int; y : int }
type direction = Up | Right | Down | Left
type position = { coord : coord; heading : direction; straight : int }
type city = { grid : int array array; nrows : int; ncols : int}
type state = { position : position; heat_loss : int }

module PositionSet = Set.Make(
  struct
    type t = position
    let compare = Stdlib.compare
  end)

module H = Heap.Make(
  struct
    type t = state
    let compare {position = p1; heat_loss = h1} {position = p2; heat_loss = h2} = compare (h1, p1) (h2, p2)
  end)


let parse_city = function
  | [] -> failwith "Empty input"
  | (h :: _) as lines ->
      let nrows = List.length lines in
      let ncols = String.length h in
      let parse_row row =
        row |> String.to_seq |> Seq.map (fun c -> int_of_string @@ String.make 1 c) |> Array.of_seq in
      let grid = Array.of_list @@ List.map parse_row lines in
      {grid; nrows; ncols}


let get_neighbors ({nrows; ncols; _} : city) ({coord = {x; y}; heading; straight} : position) : position list =
  let is_valid_position {coord = {x; y}; straight; _} =
    0 <= x && x < nrows && 0 <= y && y < ncols && straight <= 10 in

  if x = -1 && y = -1 then
    [{coord = {x = 0; y = 0}; heading = Down; straight = 1}; {coord = {x = 0; y = 0}; heading = Right; straight = 1}]
  else
    let potential_neighbors =
      match heading with
        | Up ->
            [{coord = {x = x - 1; y}; heading = Up; straight = straight + 1};
            {coord = {x; y = y - 1}; heading = Left; straight = 1};
            {coord = {x; y = y + 1}; heading = Right; straight = 1}]
        | Right ->
            [{coord = {x; y = y + 1}; heading = Right; straight = straight + 1};
            {coord = {x = x - 1; y}; heading = Up; straight = 1};
            {coord = {x = x + 1; y}; heading = Down; straight = 1}]
        | Down ->
            [{coord = {x = x + 1; y}; heading = Down; straight = straight + 1};
            {coord = {x; y = y - 1}; heading = Left; straight = 1};
            {coord = {x; y = y + 1}; heading = Right; straight = 1}]
        | Left ->
            [{coord = {x; y = y - 1}; heading = Left; straight = straight + 1};
            {coord = {x = x - 1; y}; heading = Up; straight = 1};
            {coord = {x = x + 1; y}; heading = Down; straight = 1}] in
    let potential_neighbors' = if straight < 4 then [List.hd potential_neighbors] else potential_neighbors in
    List.filter is_valid_position potential_neighbors'


let calc_minimal_heat_loss ({grid; nrows; ncols} as city : city) : int =
  let is_target {coord = {x; y}; straight; _} =
    x = nrows - 1 && y = ncols - 1 && straight >= 4 in
  let rec loop finalized heap =
    let {heat_loss; position} = H.find_min heap in
    let heap' = H.del_min heap in
    if is_target position then heat_loss
    else if PositionSet.mem position finalized then loop finalized heap'
    else
      let neighbors = get_neighbors city position in
      let states =
        List.map (fun ({coord = {x; y}; _} as n) -> {position = n; heat_loss = heat_loss + grid.(x).(y)}) neighbors in
    loop (PositionSet.add position finalized) (List.fold_left H.insert heap' states) in

  let initial_state = {position = {coord = {x = -1; y = -1}; heading = Up; straight = 0}; heat_loss = -grid.(0).(0)} in
  loop PositionSet.empty H.(empty |> add initial_state)


let () =
  let filename = "../resources/input_17.txt" in
  let lines = In_channel.with_open_text filename In_channel.input_lines in
  let city = parse_city lines in
  let result = calc_minimal_heat_loss city in
  print_int result; print_newline ()
