let hash_algorithm (str : string) : int =
  String.fold_left (fun acc chr -> 17 * (acc + Char.code chr) mod 256) 0 str


let calc_sum_of_hashes (steps : string list) : int =
  List.fold_left (fun acc str -> acc + hash_algorithm str) 0 steps


let parse_steps (data : string) : string list =
  data
    |> String.to_seq
    |> Seq.filter (fun c -> c <> '\n')
    |> String.of_seq
    |> String.split_on_char ','


let () =
  let filename = "../resources/input_15.txt" in
  let data = In_channel.with_open_text filename In_channel.input_all in
  let steps =  parse_steps data in
  let result = calc_sum_of_hashes steps in
  print_int result; print_newline ()
