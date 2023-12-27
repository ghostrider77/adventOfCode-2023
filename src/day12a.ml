module List = struct
  include List

  let drop_while p xs =
    let rec loop = function
      | h :: tl when p h -> loop tl
      | ls -> ls in
    loop xs

  let span p xs =
    let rec loop acc = function
      | h :: tl when p h -> loop (h :: acc) tl
      | ls -> List.rev acc, ls in
    loop [] xs
end


type spring = Operational | Damaged | Unknown
type record = { arrangement : spring list; group_sizes : int list }

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
    {arrangement; group_sizes} in

  List.map parse lines


let rec different_arrangements ({arrangement; group_sizes} : record) : int =
  let change_first_occurrence xs s =
    let rec loop acc = function
        | [] -> List.rev acc
        | Unknown :: rest -> (List.rev (s :: acc)) @ rest
        | x :: xss -> loop (x :: acc) xss in
    loop [] xs in

  let arrangement' = List.drop_while (fun s -> s = Operational) arrangement in
  match (arrangement', group_sizes) with
    | ([], []) -> 1
    | ([], _) -> 0
    | (_ , []) -> if List.for_all (fun s -> s <> Damaged) arrangement' then 1 else 0
    | (_, size :: sizes) ->
        let block, rest = List.span (fun s -> s <> Operational) arrangement' in
        if List.for_all (fun s -> s = Damaged) block then
          if List.length block = size then different_arrangements {arrangement = rest; group_sizes = sizes}
          else 0
        else
          let a1 = change_first_occurrence arrangement' Operational in
          let a2 = change_first_occurrence arrangement' Damaged in
          (different_arrangements {arrangement = a1; group_sizes} +
            different_arrangements {arrangement = a2; group_sizes})


let () =
  let filename = "../resources/input_12.txt" in
  let lines = In_channel.with_open_text filename In_channel.input_lines in
  let records = parse_input_records lines in
  let result = List.fold_left (fun acc record -> acc + different_arrangements record) 0 records in
  print_int result; print_newline ()
