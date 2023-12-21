type coord = { x : int; y : int }
type tile = Rock | GardenPlot

module CoordSet = Set.Make(
  struct
    type t = coord
    let compare = Stdlib.compare
  end)

type grid = { garden_plots : CoordSet.t; size : int }


let parse_input = function
  | [] -> failwith "Empty input"
  | (h :: _) as lines ->
      let nrows = List.length lines in
      let ncols = String.length h in
      if nrows <> ncols then failwith "Grid is expected to be a square."
      else if not ((List.nth lines (nrows/2)).[ncols/2] = 'S') then
        failwith "Start tile is not in the middle of the grid."
      else
        let garden_plots =
          lines
            |> List.to_seq
            |> Seq.mapi (fun x line -> Seq.map (fun (y, chr) -> ({x; y}, chr)) (String.to_seqi line))
            |> Seq.concat
            |> Seq.filter (fun (_, chr) -> chr <> '#') in
        {garden_plots = CoordSet.of_seq @@ Seq.map (fun (coord, _) -> coord) garden_plots; size = nrows}


let neville_interpolation (xs : int array) (ys : int array) (x : int) =
  let n = Array.length ys in
  let table = Array.make_matrix n (n + 1) 0 in
  for k = 0 to n - 1 do
    table.(k).(0) <- xs.(k) - x;
    table.(k).(1) <- ys.(k)
  done;

  for j = 2 to n do
    for i = 0 to n - j do
      let d = table.(i).(j-1) * table.(i+j-1).(0) - table.(i+1).(j-1) * table.(i).(0) in
      table.(i).(j) <- d / (table.(i+j-1).(0) - table.(i).(0));
    done;
  done;
  table.(0).(n)


let get_neighbors ({garden_plots; size} : grid) ({x; y} : coord) : coord list =
  let neighbors = [{x = x - 1; y}; {x; y = y + 1}; {x = x + 1; y}; {x; y = y - 1}] in
  let modulo n m =
    let n' = n mod m in
    if n' >= 0 then n' else n' + m in
  List.filter (fun {x; y} -> CoordSet.mem {x = modulo x size; y = modulo y size} garden_plots) neighbors


let count_garden_tiles (grid : grid) (start_tile : coord) (nr_steps : int) : int =
  let rec loop current_plots k =
    if k = nr_steps then CoordSet.cardinal current_plots
    else
      let add_neighbors coord acc =
        let ns = get_neighbors grid coord in
        CoordSet.add_seq (List.to_seq ns) acc in
      let acc' = CoordSet.fold add_neighbors current_plots CoordSet.empty in
      loop acc' (k + 1) in

  loop (CoordSet.singleton start_tile) 0


let count_large_garden_tiles ({size; _ } as grid : grid) (nr_steps : int) : int =
  let start_tile = {x = size / 2; y = size / 2} in
  let half_size = size / 2 in
  let xs = [|half_size; half_size+size; half_size+2*size|] in
  let ys = Array.map (count_garden_tiles grid start_tile) xs in
  neville_interpolation xs ys nr_steps


let () =
  let filename = "../resources/input_21.txt" in
  let lines = In_channel.with_open_text filename In_channel.input_lines in
  let garden_plot = parse_input lines in
  let steps = 26501365 in
  let result = count_large_garden_tiles garden_plot steps in
  print_int result; print_newline ()
