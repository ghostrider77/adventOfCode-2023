module String = struct
  include String

  let drop_while p str =
    str |> to_seq |> Seq.drop_while p |> of_seq

  let span p str =
    match Seq.find_index (fun c -> not (p c)) (to_seq str) with
      | Some ix -> (sub str 0 ix, sub str ix (length str - ix))
      | None -> (str, "")

end

type record = { arrangement : string; group_sizes : int list }

let parse_input_records (lines : string list) : record list =
  let parse line =
    let arrangement, groups = Scanf.sscanf line "%s %s" (fun a g -> a, g) in
    let group_sizes = groups |> String.split_on_char ',' |> List.map int_of_string in
    {arrangement; group_sizes} in

  let unfold_record { arrangement; group_sizes } =
    let unfolded_sizes = List.flatten @@ List.init 5 (fun _ -> group_sizes) in
    let unfolded_arrangement = String.concat "?" @@ List.init 5 (fun _ -> arrangement) in
    {arrangement = unfolded_arrangement; group_sizes = unfolded_sizes} in

  List.map (fun line -> unfold_record (parse line)) lines


let different_arrangements (record : record) : int =
  let change_first_occurrence str chr =
    match String.index_opt str '?' with
      | Some ix ->
          let bs = String.to_bytes str in
          Bytes.set bs ix chr;
          Bytes.to_string bs
      | None -> str in

  let cache = Hashtbl.create 1000 in
  let rec calc_arrangements {arrangement; group_sizes} =
    let arrangement' = String.drop_while (fun s -> s = '.') arrangement in
    match (String.length arrangement', group_sizes) with
      | (0, []) -> 1
      | (0, _) -> 0
      | (_ , []) -> if String.for_all (fun s -> s <> '#') arrangement' then 1 else 0
      | (_, size :: sizes) ->
          let current_record = {arrangement = arrangement'; group_sizes} in
          match Hashtbl.find_opt cache current_record with
            | Some n -> n
            | None ->
                let block, rest = String.span (fun s -> s <> '.') arrangement' in
                let result =
                  if String.for_all (fun s -> s = '#') block then
                    if String.length block = size then calc_arrangements {arrangement = rest; group_sizes = sizes}
                    else 0
                  else
                    let a1 = change_first_occurrence arrangement' '.' in
                    let a2 = change_first_occurrence arrangement' '#' in
                    (calc_arrangements {arrangement = a1; group_sizes} +
                      calc_arrangements {arrangement = a2; group_sizes}) in
                Hashtbl.add cache current_record result;
                result in

  calc_arrangements record


let () =
  let filename = "../resources/input_12.txt" in
  let lines = In_channel.with_open_text filename In_channel.input_lines in
  let records = parse_input_records lines in
  let result = List.fold_left (fun acc record -> acc + different_arrangements record) 0 records in
  print_int result; print_newline ()
