open Num

type position = { x : num; y : num; z : num }
type speed = { v_x : num; v_y : num; v_z : num }
type hail = { position : position; speed : speed }


let create_matrix ({position = p1; speed = v1} : hail)
                  ({position = p2; speed = v2} : hail)
                  ({position = p3; speed = v3} : hail) : num array array * num array  =
  let matrix = [|
    [|Int 0; v2.v_z -/ v1.v_z; v1.v_y -/ v2.v_y; Int 0; p1.z -/ p2.z; p2.y -/ p1.y|];
    [|v2.v_z -/ v1.v_z; Int 0; v1.v_x -/ v2.v_x; p1.z -/ p2.z; Int 0; p2.x -/ p1.x|];
    [|v2.v_y -/ v1.v_y; v1.v_x -/ v2.v_x; Int 0; p1.y -/ p2.y; p2.x -/ p1.x; Int 0|];
    [|Int 0; v3.v_z -/ v1.v_z; v1.v_y -/ v3.v_y; Int 0; p1.z -/ p3.z; p3.y -/ p1.y|];
    [|v3.v_z -/ v1.v_z; Int 0; v1.v_x -/ v3.v_x; p1.z -/ p3.z; Int 0; p3.x -/ p1.x|];
    [|v3.v_y -/ v1.v_y; v1.v_x -/ v3.v_x; Int 0; p1.y -/ p3.y; p3.x -/ p1.x; Int 0|];
  |] in
  let b = [|
    (p1.z */ v1.v_y -/ p2.z */ v2.v_y) -/ (p1.y */ v1.v_z -/ p2.y */ v2.v_z);
    (p1.z */ v1.v_x -/ p2.z */ v2.v_x) -/ (p1.x */ v1.v_z -/ p2.x */ v2.v_z);
    (p1.y */ v1.v_x -/ p2.y */ v2.v_x) -/ (p1.x */ v1.v_y -/ p2.x */ v2.v_y);
    (p1.z */ v1.v_y -/ p3.z */ v3.v_y) -/ (p1.y */ v1.v_z -/ p3.y */ v3.v_z);
    (p1.z */ v1.v_x -/ p3.z */ v3.v_x) -/ (p1.x */ v1.v_z -/ p3.x */ v3.v_z);
    (p1.y */ v1.v_x -/ p3.y */ v3.v_x) -/ (p1.x */ v1.v_y -/ p3.x */ v3.v_y);
  |] in
  matrix, b


let calc_determinant (matrix: num array array) : num =
  let rec loop = function
    | [] -> failwith "Empty matrix."
    | [[a]] -> a
    | first_row :: rest ->
        let process det (a, jy) =
          if a = Int 0 then det
          else
            let submatrix = List.map (fun row -> List.filteri (fun i _ -> i <> jy) row) rest in
            let sign = if jy mod 2 = 0 then 1 else -1 in
            det +/ (Int sign) */ a */ loop submatrix in
        List.fold_left process (Int 0) (List.mapi (fun jy a -> (a, jy)) first_row)
  in loop Array.(matrix |> map to_list |> to_list)


let cramers_rule (matrix : num array array) (b : num array) : num array option =
  let det = calc_determinant matrix in
  if det = Int 0 then None
  else
    let n = Array.length matrix in
    let replace_column j =
      let matrix' = Array.(map copy matrix) in
      for i = 0 to (n - 1) do
        matrix'.(i).(j) <- b.(i);
      done;
      matrix' in
    let calculate_component j =
      let matrix' = replace_column j in
      (calc_determinant matrix') // det in
    Some (Array.init n calculate_component)


let parse_input (lines : string list) : hail list =
  let parse line =
    Scanf.sscanf line "%d, %d, %d @ %d, %d, %d"
      (fun x y z v_x v_y v_z ->
        {position = {x = Int x; y = Int y; z = Int z}; speed = {v_x = Int v_x; v_y = Int v_y; v_z = Int v_z}}) in
  List.map parse lines


let rec find_rock_position = function
  | h1 :: h2 :: h3 :: hs ->
      let matrix, b = create_matrix h1 h2 h3 in
      (match cramers_rule matrix b with
        | Some x -> (x.(0) +/ x.(1) +/ x.(2)) |> string_of_num
        | None -> find_rock_position (h2 :: h3 :: hs))
  | _ -> failwith "Need the first 3 hails"


let () =
  let filename = "../resources/input_24.txt" in
  let lines = In_channel.with_open_text filename In_channel.input_lines in
  let hails = parse_input lines in
  let result = find_rock_position hails in
  print_endline result
