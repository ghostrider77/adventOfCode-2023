type interval = { a : int; b : int }
type brick = { x : interval; y : interval; z : interval }

module BrickSet = Set.Make(
  struct
    type t = brick
    let compare {x = x1; y = y1; z = z1} {x = x2; y = y2; z = z2} = compare (z1, x1, y1) (z2, x2, y2)
  end)


module SupportMap = Map.Make(
  struct
    type t = brick
    let compare {x = x1; y = y1; z = z1} {x = x2; y = y2; z = z2} = compare (z1, x1, y1) (z2, x2, y2)
  end)


let parse_input (lines : string list) : brick list =
  let parse_brick line =
    Scanf.sscanf line "%d,%d,%d~%d,%d,%d"
    (fun x0 y0 z0 x1 y1 z1 -> {x = {a = x0; b = x1}; y = {a = y0; b = y1}; z = {a = z0; b = z1}}) in

  List.map parse_brick lines


let is_interfering ({x; y; _} : brick) ({x = x'; y = y'; _} : brick) : bool =
  let intersect_x = x'.b >= x.a && x.b >= x'.a in
  let intersect_y = y'.b >= y.a && y.b >= y'.a in
  intersect_x && intersect_y


let move_brick ({z = {a = z0; b = z1}; _} as brick : brick) (standing_bricks : BrickSet.t) : brick =
  let interfering_bricks = BrickSet.filter (is_interfering brick) standing_bricks in
  let {b = z1'; _} =
    interfering_bricks
      |> BrickSet.to_seq
      |> Seq.map (fun {z; _} -> z)
      |> Seq.fold_left (fun acc interval -> if interval.b > acc.b then interval else acc) {a = 0; b = 0} in
  {brick with z = {a = z1' + 1; b = z1 - z0 + z1' + 1}}


let move_each_brick (snapshot : BrickSet.t) : BrickSet.t =
  let rec loop acc bricks =
    match BrickSet.min_elt_opt bricks with
      | None -> acc
      | Some brick ->
          let brick' = move_brick brick acc in
          loop (BrickSet.add brick' acc) (BrickSet.remove brick bricks) in
  loop BrickSet.empty snapshot


let fall_bricks (initial_snapshot : brick list) : BrickSet.t =
  let rec loop snapshot =
    let snapshot' = move_each_brick snapshot in
    if snapshot' = snapshot then snapshot
    else loop snapshot' in
  loop (BrickSet.of_list initial_snapshot)


let collect_supports (bricks : BrickSet.t) : (BrickSet.t SupportMap.t * BrickSet.t SupportMap.t) =
  let does_support ({z = {b = z1; _}; _} as brick1) ({z = {a = z0'; _}; _} as brick2) =
    is_interfering brick1 brick2 && z1 + 1 = z0' in
  let process brick (acc1, acc2) =
    let supports = BrickSet.filter (does_support brick) bricks in
    let supported_by = BrickSet.filter (Fun.flip does_support brick) bricks in
    (SupportMap.add brick supports acc1, SupportMap.add brick supported_by acc2) in

  BrickSet.fold process bricks (SupportMap.empty, SupportMap.empty)


let falling_bricks (brick : brick) (supports : BrickSet.t SupportMap.t) (supported_by : BrickSet.t SupportMap.t) : int =
  let immediate_supports = SupportMap.find brick supports in
  let next_bricks_to_fall =
    BrickSet.filter (fun s -> BrickSet.cardinal (SupportMap.find s supported_by) = 1) immediate_supports in
  let queue = Queue.create () in
  Queue.add_seq queue (BrickSet.to_seq next_bricks_to_fall);
  let rec loop count visited =
    match Queue.take_opt queue with
      | None -> count
      | Some falling_brick ->
          let supps = BrickSet.diff (SupportMap.find falling_brick supports) visited in
          let bricks = BrickSet.filter (fun s -> BrickSet.subset (SupportMap.find s supported_by) visited) supps in
          Queue.add_seq queue (BrickSet.to_seq bricks);
          loop (count + 1) (BrickSet.union visited bricks) in
  loop 0 next_bricks_to_fall


let calc_total_number_of_falling_bricks (bricks : brick list) : int =
  let settled = fall_bricks bricks in
  let supports, supported_by = collect_supports settled in
  BrickSet.fold (fun brick acc -> acc + falling_bricks brick supports supported_by) settled 0


let () =
  let filename = "../resources/input_22.txt" in
  let lines = In_channel.with_open_text filename In_channel.input_lines in
  let bricks = parse_input lines in
  let result = calc_total_number_of_falling_bricks bricks in
  print_int result; print_newline ()
