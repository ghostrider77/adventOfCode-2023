type coord = { x : int; y : int }
type direction = Up | Right | Down | Left
type instruction = { direction : direction; steps : int }


let direction_of_char = function
  | '3' -> Up
  | '0' -> Right
  | '1' -> Down
  | '2' -> Left
  | _ -> failwith "Unknown direction"


let parse_input (lines : string list) : instruction list =
  let parse line =
    let encoded_instruction = Scanf.sscanf line "%c %d (#%s@)" (fun _ _ s -> s) in
    let n = String.length encoded_instruction in
    let direction = direction_of_char @@ encoded_instruction.[n-1] in
    { direction; steps = int_of_string @@ "0x" ^ String.sub encoded_instruction 0 (n - 1) } in
  List.map parse lines


let move ({x; y} : coord) ({direction; steps; _} : instruction) : coord =
  match direction with
    | Up -> {x = x - steps; y}
    | Right -> {x; y = y + steps}
    | Down -> {x = x + steps; y}
    | Left -> {x; y = y - steps}


let determine_vertices (instructions : instruction list) : coord list =
  let rec loop acc vertex = function
    | [] -> List.rev acc
    | d :: ds ->
        let vertex' = move vertex d in
        loop (vertex :: acc) vertex' ds in
  loop [] {x = 0; y = 0} instructions


let calc_boundary_length (path : coord array) : int =
  let n = Array.length path in
  let rec loop acc ix =
    if ix = n then acc
    else
      let ix' = if ix = n - 1 then 0 else ix + 1 in
      let {x = x1; y = y1} = path.(ix) in
      let {x = x2; y = y2} = path.(ix') in
      let acc' = acc + abs (x2 - x1) + abs (y2 - y1) in
      loop acc' (ix + 1) in
  loop 0 0


let calc_interior_points (path : coord array) (nr_boundary_points : int) : int =
  let n = Array.length path in
  let process acc (ix, {x; _}) =
    if ix = 0 then acc + x * (path.(1).y - path.(n - 1).y)
    else if ix = n - 1 then acc + x * (path.(0).y - path.(n - 2).y)
    else acc + x * (path.(ix + 1).y - path.(ix - 1).y) in
  let double_area = abs @@ Array.fold_left process 0 @@ Array.mapi (fun ix item -> (ix, item)) path in
  (double_area - nr_boundary_points + 2) / 2


let calculate_volume (instructions : instruction list) : int =
  let path = Array.of_list @@ determine_vertices instructions in
  let nr_boundary_points = calc_boundary_length path in
  let nr_interior_points = calc_interior_points path nr_boundary_points in
  nr_boundary_points + nr_interior_points


let () =
  let filename = "../resources/input_18.txt" in
  let lines = In_channel.with_open_text filename In_channel.input_lines in
  let instructions = parse_input lines in
  let result = calculate_volume instructions in
  print_int result; print_newline ()
