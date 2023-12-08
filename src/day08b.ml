module StringMap = Map.Make(String)
module StringSet = Set.Make(String)

type instruction = Left | Right
type network = (string * string) StringMap.t

let instruction_of_char = function
  | 'L' -> Left
  | 'R' -> Right
  | _ -> failwith "Unknown instruction."


let read_lines (filename : string) : string list =
  In_channel.with_open_text filename (In_channel.input_lines)


let calc_lcm (n : int) (m : int) =
  let rec calc_gcd a b =
    if b = 0 then a else calc_gcd b (a mod b) in
  let d = calc_gcd n m in
  (n / d) * m


let parse_input (lines : string list) : instruction list * network =
  let read_node line =
    Scanf.sscanf line "%s = (%s@, %s@)" (fun a b c -> a, (b, c)) in
  let update network line =
    let parent, children = read_node line in
    StringMap.add parent children network in
  match lines with
    | xs :: _ :: nodes ->
      let instructions = xs |> String.to_seq |> Seq.map instruction_of_char |> List.of_seq in
      let network = List.fold_left update StringMap.empty nodes in
      (instructions, network)
    | _ -> failwith "Malformed input."


let traverse_network (start_node : string) (network : network) (instructions : instruction list) : int =
  let rec loop node ds k =
    if String.ends_with ~suffix:"Z" node then k
    else
      let (left, right) = StringMap.find node network in
      match Seq.uncons ds with
        | Some (Left, dss) -> loop left dss (k + 1)
        | Some (Right, dss) -> loop right dss (k + 1)
        | None -> failwith "Instructions are completly consumed." in
  loop start_node (Seq.cycle @@ List.to_seq instructions) 0


let traverse_network_simultaneously (network : network) (instructions : instruction list) : int =
  let collect_nodes chr =
    let add_valid_nodes parent (left, right) acc =
      let nodes = Seq.filter (String.ends_with ~suffix:chr) @@ List.to_seq [parent; left; right] in
      StringSet.add_seq nodes acc in
    StringMap.fold add_valid_nodes network StringSet.empty in
  let start_nodes = collect_nodes "A" in
  StringSet.fold (fun node acc -> calc_lcm acc (traverse_network node network instructions)) start_nodes 1


let () =
  let lines = read_lines "../resources/input_08.txt" in
  let (instructions, network) = parse_input lines in
  let result = traverse_network_simultaneously network instructions in
  print_int result; print_newline ()
