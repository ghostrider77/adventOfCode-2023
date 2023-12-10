let convert_to_intlist (line : string) : int list =
  List.map int_of_string Str.(line |> split (regexp "[ \t]+"))


let neville_interpolation (ys : int array) (x : int) =
  let n = Array.length ys in
  let table = Array.make_matrix n (n + 1) 0 in
  for k = 0 to n - 1 do
    table.(k).(0) <- k - x;
    table.(k).(1) <- ys.(k)
  done;

  for j = 2 to n do
    for i = 0 to n - j do
      let d = table.(i).(j-1) * table.(i+j-1).(0) - table.(i+1).(j-1) * table.(i).(0) in
      table.(i).(j) <- d / (table.(i+j-1).(0) - table.(i).(0));
    done;
  done;
  table.(0).(n)


let sum_of_extrapolated_values (histories : int list list) : int =
  List.fold_left (fun acc h -> acc + neville_interpolation (Array.of_list h) (List.length h)) 0 histories


let () =
  let filename = "../resources/input_09.txt" in
  let lines = In_channel.with_open_text filename In_channel.input_lines in
  let histories = List.map convert_to_intlist lines in
  let result = sum_of_extrapolated_values histories in
  print_int result; print_newline ();
