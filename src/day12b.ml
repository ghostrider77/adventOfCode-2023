module Array = struct
  include Array

  let drop_while p arr =
    match find_index (fun x -> not (p x)) arr with
      | Some ix -> sub arr ix (length arr - ix)
      | None -> [||]

  let span p arr =
    match find_index (fun x -> not (p x)) arr with
      | None -> (arr, [||])
      | Some ix -> (sub arr 0 ix, sub arr ix (length arr - ix))
end

type spring = Operational | Damaged | Unknown
type record = { arrangement : spring array; group_sizes : int list }

let spring_of_char = function
  | '.' -> Operational
  | '#' -> Damaged
  | '?' -> Unknown
  | _ -> failwith "Unknown spring condition."


let parse_input_records (lines : string list) : record list =
  let parse line =
    let spring_conditions, groups = Scanf.sscanf line "%s %s" (fun a g -> a, g) in
    let arrangement = spring_conditions |> String.to_seq |> Seq.map spring_of_char |> Array.of_seq in
    let group_sizes = groups |> String.split_on_char ',' |> List.map int_of_string in
    { arrangement; group_sizes } in

  let unfold_record { arrangement; group_sizes } =
    let unfolded_sizes = List.flatten @@ List.init 5 (fun _ -> group_sizes) in
    let unfolded_arrangement = List.tl @@ List.flatten @@ List.init 5 (fun _ -> Unknown :: Array.to_list arrangement) in
    { arrangement = Array.of_list unfolded_arrangement; group_sizes = unfolded_sizes } in

  List.map (fun line -> unfold_record (parse line)) lines


let different_arrangements (record : record) : int =
  let change_first_occurrence arr s =
    match Array.find_index (fun x -> x = Unknown) arr with
      | Some ix ->
          let arr' = Array.copy arr in
          arr'.(ix) <- s;
          arr'
      | None -> arr in
  let cache = Hashtbl.create 1000 in
  let rec calc_arrangements {arrangement; group_sizes} =
    let arrangement' = Array.drop_while (fun s -> s = Operational) arrangement in
    match (Array.length arrangement', group_sizes) with
      | (0, []) -> 1
      | (0, _) -> 0
      | (_ , []) -> if Array.for_all (fun s -> s <> Damaged) arrangement' then 1 else 0
      | (_, size :: sizes) ->
          let current_record = {arrangement = arrangement'; group_sizes} in
          match Hashtbl.find_opt cache current_record with
            | Some n -> n
            | None ->
                let block, rest = Array.span (fun s -> s <> Operational) arrangement' in
                let result =
                  if Array.for_all (fun s -> s = Damaged) block then
                    if Array.length block = size then calc_arrangements {arrangement = rest; group_sizes = sizes}
                    else 0
                  else
                    let a1 = change_first_occurrence arrangement' Operational in
                    let a2 = change_first_occurrence arrangement' Damaged in
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
