type rating = { x : int; m : int; a : int; s : int }
type verdict = Accepted | Rejected
type condition = { field : char; op : int -> int -> bool; rhs : int }
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
          let cond = if op = '<' then {field; op = (<); rhs} else {field; op = (>); rhs} in
          IfThen (cond, parse_result name)
      | _ -> failwith "Malformed input." in

  (name, List.map parse_rule items)


let parse_input (lines : string list) : rating list * (rule list) StringMap.t =
  let parse_rating (line : string) : rating =
    Scanf.sscanf line "{x=%d,m=%d,a=%d,s=%d}" (fun x m a s -> {x; m; a; s}) in
  let workflow_lines = lines |> List.to_seq |> Seq.take_while (fun line -> line <> "") |> List.of_seq in
  let rating_lines = lines |> List.to_seq |> Seq.drop_while (fun line -> line <> "") |> List.of_seq |> List.tl in
  let ratings = List.map parse_rating rating_lines in
  let process acc line =
    let (name, rules) = parse_workflow line in
    StringMap.add name rules acc in
  let workflows = List.fold_left process StringMap.empty workflow_lines in
  ratings, workflows


let check_condition ({field; op; rhs} : condition) ({x; m; a; s} : rating) : bool =
  match field with
    | 'x' -> op x rhs
    | 'm' -> op m rhs
    | 'a' -> op a rhs
    | 's' -> op s rhs
    | _ -> failwith "Unknown part rating field."


let evaluate_workflow (part_rating : rating) (rules : rule list) : result =
  let rec loop = function
    | (IfThen (cond, r)) :: rs ->
        if check_condition cond part_rating then r
        else loop rs
    | [Else r] -> r
    | _ -> failwith "Failed to evaluate workflow" in
  loop rules


let get_verdict (part_rating : rating) (workflows : rule list StringMap.t) : verdict =
  let rec loop workflow =
    match evaluate_workflow part_rating workflow with
      | Verdict Accepted -> Accepted
      | Verdict Rejected -> Rejected
      | WorkFlowName name ->
          let workflow' = StringMap.find name workflows in
          loop workflow' in
  loop (StringMap.find "in" workflows)


let get_accepted_ratings (ratings : rating list) (workflows : rule list StringMap.t) : int =
  let process_rating acc ({x; m; a; s} as r) =
    match get_verdict r workflows with
      | Accepted -> acc + (x + m + a + s)
      | Rejected -> acc in
  List.fold_left process_rating 0 ratings


let () =
  let filename = "../resources/input_19.txt" in
  let lines = In_channel.with_open_text filename In_channel.input_lines in
  let ratings, workflows = parse_input lines in
  let result = get_accepted_ratings ratings workflows in
  print_int result; print_newline ()
