type pulse = High | Low
type module_state = On | Off
type message = { sender : string; receiver : string; pulse : pulse }

module StringMap = Map.Make(String)

type cmodule =
  | Button
  | BroadCast
  | Untyped
  | FlipFlop of module_state
  | Conjunction of pulse StringMap.t

module DSM : sig
  type t
  val create : cmodule StringMap.t -> string list StringMap.t -> t
  val receive_message : message -> t -> t * message list
end = struct
  type t = { states : cmodule StringMap.t; adjacency_list : string list StringMap.t }

  let get_or_else mapping key =
    match StringMap.find_opt key mapping with
      | None -> []
      | Some ns -> ns


  let create states adjacency_list =
    {states; adjacency_list}


  let receive_message {sender; receiver; pulse} ({states; adjacency_list} as ms) =
    match StringMap.find_opt receiver states with
      | None -> (ms, [])
      | Some state ->
          let state', optional_pulse =
            match (state, pulse) with
              | (BroadCast, _) -> (BroadCast, Some pulse)
              | (FlipFlop st, High) -> (FlipFlop st, None)
              | (FlipFlop On, Low) -> (FlipFlop Off, Some Low)
              | (FlipFlop Off, Low) -> (FlipFlop On, Some High)
              | (Conjunction received, _) ->
                  let received' = StringMap.add sender pulse received in
                  let m' = Conjunction received' in
                  if StringMap.for_all (fun _ p -> p = High) received' then (m', Some Low)
                  else (m', Some High)
              | (Untyped, _) -> (Untyped, None)
              | _ -> failwith "This state cannot receive messages" in
          let states' = StringMap.add receiver state' states in
          let ms' = {ms with states = states'} in
          match optional_pulse with
            | None -> (ms', [])
            | Some emitted_pulse ->
                let neighbors = get_or_else adjacency_list receiver in
                let messages = List.map (fun n -> {sender = receiver; receiver = n; pulse = emitted_pulse}) neighbors in
                (ms', messages)
end


let reverse (mapping : string list StringMap.t) : string list StringMap.t =
  let add_node key value m =
    let update = function
      | None -> Some [value]
      | Some ps -> Some (value :: ps) in
    StringMap.update key update m in
  let rec loop acc = function
    | (node, neighbors) :: rest ->
        let acc' = List.fold_left (fun a n -> add_node n node a) acc neighbors in
        loop acc' rest
    | [] -> acc in
  loop StringMap.empty (StringMap.bindings mapping)



let parse_input (lines : string list) : cmodule StringMap.t * string list StringMap.t =
  let get_node_name node =
    if node.[0] = '%' || node.[0] = '&' then String.sub node 1 (String.length node - 1)
    else node in
  let split_line line =
    match Str.(line |> split (regexp " -> ")) with
      | [node; rest] ->
          let neighbors = Str.(rest |> split (regexp ", ")) in
          node, neighbors
      | _ -> failwith "Malformed input." in

  let collect_neighbors neighbors line =
    let node, ns = split_line line in
    let node_name = get_node_name node in
    StringMap.add node_name ns neighbors in

  let neighbors = List.fold_left collect_neighbors StringMap.empty lines in
  let parents = reverse neighbors in

  let collect_modules modules line =
    let node, _ = split_line line in
    let node_name = get_node_name node in
    let m =
      if node = "broadcaster" then BroadCast
      else if node.[0] = '%' then FlipFlop Off
      else if node.[0] = '&' then
        let incoming_nodes = (StringMap.find node_name parents) in
        Conjunction (StringMap.of_list @@ List.map (fun n -> (n, Low)) incoming_nodes)
      else Untyped in
    StringMap.add node_name m modules in

  let modules = List.fold_left collect_modules StringMap.empty lines in
  (modules, neighbors)


let calc_lcm (a : int) (b : int) : int =
  let rec calc_gcd n m =
    if m = 0 then n else calc_gcd m (n mod m) in
  let d = calc_gcd a b in
  (a / d) * b


let emit_pulses (initial_state : DSM.t) : string list * DSM.t =
  let queue = Queue.create () in
  Queue.add { sender = "button"; receiver = "broadcaster"; pulse = Low } queue;
  let rec loop state ancestors =
    match Queue.take_opt queue with
      | None -> ancestors, state
      | Some ({sender; receiver; pulse; _} as message) ->
          let state', next_messages = DSM.receive_message message state in
          Queue.add_seq queue (List.to_seq next_messages);
          match pulse with
            | High when receiver = "jm" ->
                if List.mem sender ancestors then loop state' ancestors
                else loop state' (sender :: ancestors)
            | _ -> loop state' ancestors in
  loop initial_state []


let emit_pulses_in_cycles (states : cmodule StringMap.t) (adjacency_list : string list StringMap.t) : int =
  let rec loop state counts k =
    let ancestors, state' = emit_pulses state in
    let counts' =
      List.fold_left (fun acc a -> if StringMap.mem a counts then acc else StringMap.add a k acc) counts ancestors in
    if StringMap.cardinal counts' = 4 then StringMap.fold (fun _ n acc -> calc_lcm acc n) counts' 1
    else loop state' counts' (k + 1) in

  let initial_state = DSM.create states adjacency_list in
  loop initial_state StringMap.empty 1


let () =
  let filename = "../resources/input_20.txt" in
  let lines = In_channel.with_open_text filename In_channel.input_lines in
  let modules, neighbors = parse_input lines in
  let result = emit_pulses_in_cycles modules neighbors in
  print_int result; print_newline ()
