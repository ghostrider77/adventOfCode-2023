module StringMap = Map.Make(String)

type instruction = Left | Right
type network = (string * string) StringMap.t


let instruction_of_char = function
  | 'L' -> Left
  | 'R' -> Right
  | _ -> failwith "Unknown instruction."


let parse_input (lines : string list) : instruction Seq.t * network =
  let read_node line =
    Scanf.sscanf line "%s = (%s@, %s@)" (fun a b c -> a, (b, c)) in
  let update network line =
    let parent, children = read_node line in
    StringMap.add parent children network in
  match lines with
    | xs :: _ :: nodes ->
        let instructions = xs |> String.to_seq |> Seq.map instruction_of_char in
        let network = List.fold_left update StringMap.empty nodes in
        (instructions, network)
    | _ -> failwith "Malformed input."


let traverse_network (network : network) (instructions : instruction Seq.t) : int =
  let rec loop node ds k =
    if node = "ZZZ" then k
    else
      let (left, right) = StringMap.find node network in
      match Seq.uncons ds with
        | Some (Left, dss) -> loop left dss (k + 1)
        | Some (Right, dss) -> loop right dss (k + 1)
        | None -> failwith "Instructions are completly consumed." in
  loop "AAA" (Seq.cycle instructions) 0


let () =
  let filename = "../resources/input_08.txt" in
  let lines = In_channel.with_open_text filename In_channel.input_lines in
  let (instructions, network) = parse_input lines in
  let result = traverse_network network instructions in
  print_int result; print_newline ()
