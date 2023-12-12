type spring = Operational | Damaged | Unknown
type record = { arrangement : spring list; group_sizes : int list }

module RecordTbl = Hashtbl.Make(
  struct
    type t = record
    let equal r1 r2 = r1.arrangement = r2.arrangement && r1.group_sizes = r2.group_sizes
    let hash {arrangement; group_sizes} = Hashtbl.hash (arrangement, group_sizes)
  end)


let spring_of_char = function
  | '.' -> Operational
  | '#' -> Damaged
  | '?' -> Unknown
  | _ -> failwith "Unknown spring condition."


let parse_input_records (lines : string list) : record list =
  let parse line =
    let spring_conditions, groups = Scanf.sscanf line "%s %s" (fun a g -> a, g) in
    let arrangement = spring_conditions |> String.to_seq |> Seq.map spring_of_char |> List.of_seq in
    let group_sizes = groups |> String.split_on_char ',' |> List.map int_of_string in
    { arrangement; group_sizes } in

  let unfold_record { arrangement; group_sizes } =
    let unfolded_sizes = List.flatten @@ List.init 5 (fun _ -> group_sizes) in
    let unfolded_arrangement = List.tl @@ List.flatten @@ List.init 5 (fun _ -> Unknown :: arrangement) in
    { arrangement = unfolded_arrangement; group_sizes = unfolded_sizes } in

  List.map (fun line -> unfold_record (parse line)) lines


let different_arrangements (record : record) : int =
  let change_first_occurrence xs s =
    let rec loop acc = function
        | [] -> List.rev acc
        | Unknown :: rest -> (List.rev (s :: acc)) @ rest
        | x :: xss -> loop (x :: acc) xss in
    loop [] xs in

  let cache = RecordTbl.create 10000 in
  let open Batteries in
  let rec calc_arrangements {arrangement; group_sizes} =
    let arrangement' = List.drop_while (fun s -> s = Operational) arrangement in
    match (arrangement', group_sizes) with
      | ([], []) -> 1
      | ([], _) -> 0
      | (_ , []) -> if List.for_all (fun s -> s <> Damaged) arrangement' then 1 else 0
      | (_, size :: sizes) ->
          let current_record = {arrangement = arrangement'; group_sizes} in
          match RecordTbl.find_opt cache current_record with
            | Some n -> n
            | None ->
                let block, rest = List.span (fun s -> s <> Operational) arrangement' in
                let result =
                  if List.for_all (fun s -> s = Damaged) block then
                    if List.length block = size then calc_arrangements {arrangement = rest; group_sizes = sizes}
                    else 0
                  else
                    let a1 = change_first_occurrence arrangement' Operational in
                    let a2 = change_first_occurrence arrangement' Damaged in
                    (calc_arrangements {arrangement = a1; group_sizes} +
                      calc_arrangements {arrangement = a2; group_sizes}) in
                RecordTbl.add cache current_record result;
                result in

  calc_arrangements record


let () =
  let filename = "../resources/input_12.txt" in
  let lines = In_channel.with_open_text filename In_channel.input_lines in
  let records = parse_input_records lines in
  let result = List.fold_left (fun acc record -> acc + different_arrangements record) 0 records in
  print_int result; print_newline ()
