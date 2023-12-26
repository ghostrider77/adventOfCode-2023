type edge = { src : string; dest : string }
type node = { name : string; original : edge }

module StringMap = Map.Make(String)
module StringSet = Set.Make(String)

module Graph : sig
  type t
  val create : edge list -> t
  val minimum_cut : t -> int -> edge list
  val components : t -> string list list
end = struct
  type t = {adjacency_list : node list StringMap.t}

  let get_neighbors adjacency_list str =
    match StringMap.find_opt str adjacency_list with
      | None -> []
      | Some neighbors -> List.map (fun {name; _} -> name) neighbors

  let create edges =
    let add_neighbor adjacency_list {src; dest} =
      let update_function = function
          | None -> Some [{name = dest; original = {src; dest}}]
          | Some ns -> Some ({name = dest; original = {src; dest}} :: ns) in
      StringMap.update src update_function adjacency_list in
    let add_neighbor_both_ways adjacency_list {src; dest} =
        add_neighbor (add_neighbor adjacency_list {src; dest}) {src = dest; dest = src} in
    let adjacency_list = List.fold_left add_neighbor_both_ways StringMap.empty edges in
    {adjacency_list}

  let select_binding {adjacency_list} size =
    let k = Random.int size in
    let bindings = StringMap.to_seq adjacency_list in
    match Seq.(bindings |> drop k |> uncons) with
      | Some ((node, neighbors), _) -> (node, neighbors)
      | None -> failwith "Randomly selected node index is out of bounds"


  let rec contract ({adjacency_list} as graph) =
    let size = StringMap.cardinal adjacency_list in
    if size <= 2 then List.map (fun {original; _} -> original) @@ snd @@ StringMap.min_binding adjacency_list
    else
      let u, neighbors_of_u = select_binding graph size in
      let n = List.length neighbors_of_u in
      let ix = Random.int n in
      let {name = v; _} = List.nth neighbors_of_u ix in
      let neighbors_of_v = StringMap.find v adjacency_list in
      let neighbors_of_u' = List.filter (fun {name; _} -> name <> u && name <> v) (neighbors_of_u @ neighbors_of_v) in
      let neighbors_of_v' = List.filter (fun {name; _} -> name <> u) neighbors_of_v in
      let adjacency_list' = adjacency_list
        |> StringMap.remove v
        |> StringMap.add u neighbors_of_u' in
      let update acc {name; _} =
        let ns = StringMap.find name acc in
        let ns' = List.map (fun node -> if node.name = v then {node with name = u} else node) ns in
        StringMap.add name ns' acc in
      let graph' = {adjacency_list = List.fold_left update adjacency_list' neighbors_of_v'} in
      contract graph'


  let minimum_cut graph mincut_size =
    Random.init 2112;
    let rec loop () =
      let cut = contract graph in
      if List.length cut = mincut_size then cut
      else loop () in
    loop ()

  let get_nodes {adjacency_list} =
    let get_neighbor_names ns =
      List.(ns |> map (fun {name; _} -> name) |> to_seq) in
    let nodes =
      StringMap.fold (fun name ns acc -> StringSet.(acc |> add name |> add_seq (get_neighbor_names ns)))
        adjacency_list StringSet.empty in
    StringSet.elements nodes


  let components ({adjacency_list} as graph) =
    let visit_started = Hashtbl.create 100 in
    let visit_ended = Hashtbl.create 100 in
    let previsit_id = ref 1 in
    let postvisit_id = ref 1 in
    let is_node_visited node = Hashtbl.mem visit_started node in
    let find_unvisited_neighbor node =
      node |> get_neighbors adjacency_list |> List.find_opt (fun item -> not (is_node_visited item)) in

    let explore start_node =
      let rec traverse_component component = function
        | [] -> component
        | (node :: rest) as previsit_stack ->
          match find_unvisited_neighbor node with
            | Some neighbor ->
                Hashtbl.add visit_started neighbor !previsit_id;
                previsit_id := !previsit_id + 1;
                traverse_component (neighbor :: component) (neighbor :: previsit_stack)
            | None ->
              Hashtbl.add visit_ended node !postvisit_id;
              postvisit_id := !postvisit_id + 1;
              traverse_component component rest in
        Hashtbl.add visit_started start_node !previsit_id;
        previsit_id := !previsit_id + 1;
        traverse_component [start_node] [start_node] in

    let rec find_components components = function
      | [] -> components
      | node :: remaining_nodes ->
        if is_node_visited node then find_components components remaining_nodes
        else
          let current_component = explore node in
          find_components (current_component :: components) remaining_nodes in

    find_components [] (get_nodes graph)
end


let parse_input (lines : string list) : edge list =
  let parse line =
    match String.split_on_char ':' line with
      | [src; rest] ->
          let neighbors = String.(rest |> trim |> split_on_char ' ') in
          List.map (fun dest -> {src; dest}) neighbors
      | _ -> failwith "Malformed input." in

  List.concat_map parse lines


let product_of_component_sizes (edges : edge list) (mincut_size : int) : int =
  let graph = Graph.create edges in
  let minimum_cut = Graph.minimum_cut graph mincut_size in
  let edges' =
    List.filter (fun e -> not (List.mem e minimum_cut) && not (List.mem {src = e.dest; dest = e.src} minimum_cut))
      edges in
  let reduced_graph = Graph.create edges' in
  match Graph.components reduced_graph with
    | [a; b] -> (List.length a) * (List.length b)
    | _ -> failwith "Reduced graph should have 2 components"


let () =
  let filename = "../resources/input_25.txt" in
  let lines = In_channel.with_open_text filename In_channel.input_lines in
  let edges = parse_input lines in
  let minimum_cut_size = 3 in
  let result = product_of_component_sizes edges minimum_cut_size in
  print_int result; print_newline ()
