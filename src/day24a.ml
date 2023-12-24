type position = { x : float; y : float }
type speed = { v_x : float; v_y : float }
type hail = { position : position; speed : speed }
type intersection = { t1 : float; t2 : float; x0 : float; y0 : float }
type rectangle = {x_min : float; x_max : float; y_min : float; y_max : float}


let parse_input (lines : string list) : hail list =
  let parse line =
    Scanf.sscanf line "%f, %f, %f @ %f, %f, %f" (fun x y _ v_x v_y _ -> {position = {x; y}; speed = {v_x; v_y}}) in
  List.map parse lines


let calc_position ({position = {x; y}; speed = {v_x; v_y}} : hail) (t : float) : position =
  let x' = x +. t *. v_x in
  let y' = y +. t *. v_y in
  {x = x'; y = y'}


let are_on_same_line ({position = p1; speed = v1} : hail) ({position = p2; _} : hail) : bool =
  p2.x *. v1.v_y -. p2.y *. v1.v_x = p1.x *. v1.v_y -. p1.y *. v1.v_x


let calc_intersection ({position = p1; speed = v1} : hail) ({position = p2; speed = v2} : hail) : intersection option =
  let det = v1.v_x *. (-.v2.v_y) +. v2.v_x *. v1.v_y in
  if det = 0.0 then None
  else
    let t1 = ((p2.x -. p1.x) *. -.(v2.v_y) +. v2.v_x *. (p2.y -. p1.y)) /. det in
    let t2 = (v1.v_x *. (p2.y -. p1.y) -. v1.v_y *. (p2.x -. p1.x)) /. det in
    let {x; y} = calc_position {position = p1; speed = v1} t1 in
    Some {t1; t2; x0 = x; y0 = y}


let intersect_within_area ({x_min; x_max; y_min; y_max} : rectangle) (h1 : hail) (h2 : hail) : bool =
  match calc_intersection h1 h2 with
    | None ->
        if are_on_same_line h1 h2 then failwith "Not handled, I was lazy."
        else false
    | Some {t1; t2; x0; y0} -> t1 >= 0.0 && t2 >= 0.0 && x_min <= x0 && x0 <= x_max && y_min <= y0 && y0 <= y_max


let calc_number_of_future_intersections (area : rectangle) (hails : hail list) : int =
  let rec loop acc = function
    | [] -> acc
    | h :: tl ->
        let acc' = List.fold_left (fun a h' -> if intersect_within_area area h h' then a + 1 else a) acc tl in
        loop acc' tl in
  loop 0 hails


let () =
  let filename = "../resources/input_24.txt" in
  let lines = In_channel.with_open_text filename In_channel.input_lines in
  let hails = parse_input lines in
  let min_value = 200000000000000.0 in
  let max_value = 400000000000000.0 in
  let area = {x_min = min_value; x_max = max_value; y_min = min_value; y_max = max_value} in
  let result = calc_number_of_future_intersections area hails in
  print_int result; print_newline ()
