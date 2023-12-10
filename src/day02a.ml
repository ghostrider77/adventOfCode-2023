type color = Red | Green | Blue


let string_of_color = function
  | Red -> "red"
  | Green -> "green"
  | Blue -> "blue"


let color_of_string = function
  | "red" -> Red
  | "green" -> Green
  | "blue" -> Blue
  | _ -> failwith "Unknown ball color."


module BallMap = Map.Make(
  struct
    type t = color
    let compare c1 c2 = compare (string_of_color c1) (string_of_color c2)
  end)

type game = { id : int; selections : (int BallMap.t) list }


let parse_selection (str : string) : int BallMap.t =
  let read_ball s = Scanf.sscanf s "%d %s" (fun n c -> (color_of_string c, n)) in
  str |> String.split_on_char ','
      |> List.map (fun s -> read_ball (String.trim s))
      |> BallMap.of_list


let parse_games (lines : string list) : game list =
  let split_game_info line =
    match String.split_on_char ':' line with
      | [first; rest] ->
        let id = Scanf.sscanf first "Game %d" (fun id -> id) in
        (id, String.trim rest)
      | _ -> failwith "Malformed input" in

  let parse_one_game line =
    let id, content = split_game_info line in
    let rec loop acc = function
      | [] -> { id = id; selections = List.rev acc }
      | selection :: rest -> loop ((parse_selection selection) :: acc) rest in
    loop [] (content |> String.split_on_char ';' |> List.map String.trim) in

  List.map parse_one_game lines


let get_valid_game_id_sum (games : game list) : int =
  let get_or_else balls c =
    match BallMap.find_opt c balls with
      | None -> 0
      | Some n -> n in
  let is_valid_selection balls =
    get_or_else balls Red <= 12 && get_or_else balls Green <= 13 && get_or_else balls Blue <= 14 in
  let is_valid_game { selections; _ } =
    List.for_all is_valid_selection selections in
  List.fold_left (fun acc ({ id; _ } as game) -> if is_valid_game game then acc + id else acc) 0 games


let () =
  let filename = "../resources/input_02.txt" in
  let lines = In_channel.with_open_text filename In_channel.input_lines in
  let games = parse_games lines in
  let result = get_valid_game_id_sum games in
  print_int result; print_newline ()
