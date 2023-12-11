type coord = {x : int; y : int}
type expansion = { rows : int list; columns : int list }

let expansion_size = 1000000


let parse_input (lines : string list) : coord list =
  let rec loop acc x = function
    | [] -> List.(acc |> rev |> concat)
    | line :: rest ->
        let items = Seq.filter_map (fun (y, chr) -> if chr = '#' then Some ({x; y}) else None) (String.to_seqi line) in
        loop (List.of_seq items :: acc) (x + 1) rest in
  loop [] 0 lines


let get_empty_space (lines : string list) : expansion =
  let get_empty_rows rows = rows
      |> List.mapi (fun ix row -> (ix, row))
      |> List.filter_map (fun (ix, row) -> if List.for_all (fun chr -> chr = '.') row then Some ix else None) in

  let transpose rows =
    let rec loop acc xs =
      if List.for_all (fun x -> x = []) xs then List.rev acc
      else let column = List.map List.hd xs in loop (column :: acc) (List.map List.tl xs) in
    loop [] rows in

  let ls = List.map (fun line -> List.of_seq (String.to_seq line)) lines in
  let rows = get_empty_rows ls in
  let columns = get_empty_rows (transpose ls) in
  {rows; columns}


let distance ({x = x1; y = y1} : coord) ({x = x2; y = y2} : coord) ({rows; columns} : expansion) : int =
  let down = min x1 x2 in
  let up = max x1 x2 in
  let left = min y1 y2 in
  let right = max y1 y2 in
  let row_correction =
    List.fold_left (fun acc ix -> if down < ix && ix < up then acc + expansion_size -1 else acc) 0 rows in
  let column_correction =
    List.fold_left (fun acc jy -> if left < jy && jy < right then acc + expansion_size - 1 else acc) 0 columns in
  (up - down) + (right - left) + row_correction + column_correction


let calc_sum_of_shortest_paths (universes : coord list) (expansion : expansion) : int =
  let rec loop acc = function
    | [] -> acc
    | u1 :: rest -> loop (acc + List.fold_left (fun s u2 -> s + distance u1 u2 expansion) 0 rest) rest in
  loop 0 universes


let () =
  let filename = "../resources/input_11.txt" in
  let lines = In_channel.with_open_text filename In_channel.input_lines in
  let initial_universes = parse_input lines in
  let expansion = get_empty_space lines in
  let result = calc_sum_of_shortest_paths initial_universes expansion in
  print_int result; print_newline ()
