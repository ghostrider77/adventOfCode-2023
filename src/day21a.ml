type coord = { x : int; y : int }

module CoordSet = Set.Make(
  struct
    type t = coord
    let compare = Stdlib.compare
  end)


let parse_input (lines : string list) : coord *  CoordSet.t =
  let garden_plots =
    lines
      |> List.to_seq
      |> Seq.mapi (fun x line -> Seq.map (fun (y, chr) -> ({x; y}, chr)) (String.to_seqi line))
      |> Seq.concat
      |> Seq.filter (fun (_, chr) -> chr <> '#') in
  let start_cell =
    match Seq.find (fun (_, chr) -> chr = 'S') garden_plots with
      | None -> failwith "Start tile was not found."
      | Some (coord, _) -> coord in
  let garden_plot_tiles = CoordSet.of_seq @@ Seq.map (fun (coord, _) -> coord) garden_plots in
  (start_cell, garden_plot_tiles)


let get_neighbors (garden_plots : CoordSet.t) ({x; y} : coord) : coord list =
  let neighbors = [{x = x - 1; y}; {x; y = y + 1}; {x = x + 1; y}; {x; y = y - 1}] in
  List.filter (fun coord -> CoordSet.mem coord garden_plots) neighbors


let count_garden_tiles (garden_plots : CoordSet.t) (start_tile : coord) (nr_steps : int) : int =
  let rec loop current_plots k =
    if k = nr_steps then CoordSet.cardinal current_plots
    else
      let add_neighbors coord acc =
        let ns = get_neighbors garden_plots coord in
        CoordSet.add_seq (List.to_seq ns) acc in
      let acc' = CoordSet.fold add_neighbors current_plots CoordSet.empty in
      loop acc' (k + 1) in

  loop (CoordSet.singleton start_tile) 0


let () =
  let filename = "../resources/input_21.txt" in
  let lines = In_channel.with_open_text filename In_channel.input_lines in
  let start_cell, garden_plot = parse_input lines in
  let steps = 64 in
  let result = count_garden_tiles garden_plot start_cell steps in
  print_int result; print_newline ()
