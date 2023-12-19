type interval = { left : int; right : int }
type rating = { x : interval; m : interval; a : interval; s : interval }
type verdict = Accepted | Rejected
type comparator =
  | Less of int
  | Greater of int
type condition = { field : char; op : comparator }
type result =
  | Verdict of verdict
  | WorkFlowName of string
type rule =
  | IfThen of condition * result
  | Else of result

module StringMap = Map.Make(String)


let parse_workflow (line : string) : string * rule list =
  let name, str = Scanf.sscanf line "%s@{%s@}" (fun a b -> a, b) in
  let items = String.split_on_char ',' str in
  let parse_result = function
    | "A" -> Verdict Accepted
    | "R" -> Verdict Rejected
    | name -> WorkFlowName name in
  let parse_rule item =
    match String.split_on_char ':' item with
      | [str] -> Else (parse_result str)
      | [str; name] ->
          let field, op, rhs = Scanf.sscanf str "%c%c%d" (fun a b c -> a, b, c) in
          let cond = if op = '<' then {field; op = Less rhs} else {field; op = Greater rhs} in
          IfThen (cond, parse_result name)
      | _ -> failwith "Malformed input." in

  (name, List.map parse_rule items)


let parse_input (lines : string list) : rule list StringMap.t =
  let rec loop acc = function
    | [] -> failwith "Only workflow lines should be parsed."
    | line :: ls ->
        if line = "" then acc
        else
          let (name, rules) = parse_workflow line in
          loop (StringMap.add name rules acc) ls in

  loop StringMap.empty lines


let select_interval ({field; _} : condition) ({x; m; a; s} : rating) : interval =
  match field with
    | 'x' -> x
    | 'm' -> m
    | 'a' -> a
    | 's' -> s
    | _ -> failwith "Unknown part rating field."


let subset_interval (comp : comparator) ({left; right} : interval) : interval option =
  match comp with
    | Less rhs ->
        if rhs <= left then None
        else if left < rhs && rhs <= right then Some {left; right = rhs - 1}
        else Some {left; right}
    | Greater rhs ->
        if rhs >= right then None
        else if left <= rhs && rhs < right then Some {left = rhs + 1; right}
        else Some {left; right}


let split_ratings ({field; op} as cond : condition) (rating : rating) : rating option * rating option =
  let interval = select_interval cond rating in
  let replace_interval interval' =
    match field with
      | 'x' -> {rating with x = interval'}
      | 'm' -> {rating with m = interval'}
      | 'a' -> {rating with a = interval'}
      | 's' -> {rating with s = interval'}
      | _ -> failwith "Unknown part rating field." in
  let reverse = function
    | Less k -> Greater (k - 1)
    | Greater k -> Less (k + 1) in
  let r1 = Option.map replace_interval @@ subset_interval op interval in
  let r2 = Option.map replace_interval @@ subset_interval (reverse op) interval in
  (r1, r2)


let evaluate_workflow (initial_rating : rating) (rules : rule list) : (result * rating) list =
  let rec loop acc rating = function
    | IfThen (cond, res) :: rs ->
        (match split_ratings cond rating with
          | (Some rating1, Some rating2) -> loop ((res, rating1) :: acc) rating2 rs
          | (Some rating1, None) -> List.rev ((res, rating1) :: acc)
          | (None, Some rating2) -> loop acc rating2 rs
          | (None, None) -> List.rev acc)
    | [Else res] -> List.rev ((res, rating) :: acc)
    | _ -> failwith "Failed to evaluate workflow" in
  loop [] initial_rating rules


let process_possible_ratings (workflows : rule list StringMap.t) : rating list =
  let collect_valid_rating (result, rating) =
    match result with
      | Verdict Accepted -> Some rating
      | _ -> None in
  let collect_ratings_to_be_processed (result, rating) =
    match result with
      | WorkFlowName name -> Some (name, rating)
      | _ -> None in
  let queue = Queue.create () in
  let segment = {left = 1; right = 4000} in
  Queue.add ("in", {x = segment; m = segment; a = segment; s = segment}) queue;
  let rec loop acc =
    match Queue.take_opt queue with
      | None -> acc
      | Some (name, r) ->
          let workflow = StringMap.find name workflows in
          let potential_ratings = evaluate_workflow r workflow in
          let acc' = acc @ List.filter_map collect_valid_rating potential_ratings in
          let unfinished_ratings = List.filter_map collect_ratings_to_be_processed potential_ratings in
          Queue.add_seq queue (List.to_seq unfinished_ratings);
          loop acc' in
  loop []


let calc_nr_of_accepted_ratings (workflows : rule list StringMap.t) : int =
  let accepted_ratings = process_possible_ratings workflows in
  let process acc {x; m; a; s} =
    acc + (x.right - x.left + 1) * (m.right - m.left + 1) * (a.right - a.left + 1) * (s.right - s.left + 1) in
  List.fold_left process 0 accepted_ratings


let () =
  let filename = "../resources/input_19.txt" in
  let lines = In_channel.with_open_text filename In_channel.input_lines in
  let workflows = parse_input lines in
  let result = calc_nr_of_accepted_ratings workflows in
  print_int result; print_newline ()
