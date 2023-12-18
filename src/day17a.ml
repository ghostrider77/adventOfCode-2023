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

module Heap = Set.Make(
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
    0 <= x && x < nrows && 0 <= y && y < ncols && straight <= 3 in
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

  List.filter is_valid_position potential_neighbors


let calc_minimal_heat_loss ({grid; nrows; ncols} as city : city) : int =
  let is_target {coord = {x; y}; _} =
    x = nrows - 1 && y = ncols - 1 in
  let rec loop finalized heap =
    let {heat_loss; position} as state = Heap.min_elt heap in
    let heap' = Heap.remove state heap in
    if is_target position then heat_loss
    else if PositionSet.mem position finalized then loop finalized heap'
    else
      let neighbors = get_neighbors city position in
      let states =
        List.map (fun ({coord = {x; y}; _} as n) -> {position = n; heat_loss = heat_loss + grid.(x).(y)}) neighbors in
    loop (PositionSet.add position finalized) (List.fold_right Heap.add states heap') in

  let initial_state = {position = {coord = {x = 0; y = 0}; heading = Down; straight = 1}; heat_loss = 0} in
  loop PositionSet.empty Heap.(empty |> add initial_state)


let () =
  let filename = "../resources/input_17.txt" in
  let lines = In_channel.with_open_text filename In_channel.input_lines in
  let city = parse_city lines in
  let result = calc_minimal_heat_loss city in
  print_int result; print_newline ()
