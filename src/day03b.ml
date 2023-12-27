module List = struct
  include List

  let span p xs =
    let rec loop acc = function
      | h :: tl when p h -> loop (h :: acc) tl
      | ls -> List.rev acc, ls in
    loop [] xs
end

type cell = { x : int; y : int }
type symbol = { position : cell; value : char }
type number = { start : cell; length : int; value : int }

module SymbolSet = Set.Make(
  struct
    type t = symbol
    let compare {position = {x = x1; y = y1}; _} { position = {x = x2; y = y2}; _} = Stdlib.compare (x1, y1) (x2, y2)
  end
)

type engine_schema = { symbols : SymbolSet.t; numbers : number list }


let parse_single_line (line : string) (row_index : int) : symbol list * number list =
  let is_digit = function
    | '0' .. '9' -> true
    | _ -> false in
  let rec loop symbols numbers column_ix = function
    | [] -> (symbols, numbers)
    | (c :: css) as cs ->
      if c = '.' then loop symbols numbers (column_ix + 1) css
      else if is_digit c then
        let (digits, rest) = List.span is_digit cs in
        let length = List.length digits in
        let start = {x = row_index; y = column_ix} in
        let n = {start; length; value = int_of_string @@ String.concat "" (List.map (String.make 1) digits)} in
        loop symbols (n :: numbers) (column_ix + length) rest
      else
        let position = {x = row_index; y = column_ix} in
        loop ({position; value = c} :: symbols) numbers (column_ix + 1) css in
  loop [] [] 0 (List.of_seq @@ String.to_seq line)


let parse_engine_schematic (lines : string list) : engine_schema =
  let rec loop symbols numbers row_ix = function
    | [] -> { symbols; numbers = List.rev numbers }
    | line :: ls ->
        let syms, ns = parse_single_line line row_ix in
        loop (SymbolSet.union symbols (SymbolSet.of_list syms)) (ns @ numbers) (row_ix + 1) ls in
  loop SymbolSet.empty [] 0 lines


let calc_gear_ratio ({position = {x = sx; y = sy}; value} : symbol) (numbers : number list) : int option =
  let is_number_adjacent {start = {x; y}; length; _} =
    abs (sx - x) <= 1 && y - 1 <= sy && sy <= y + length in
  if value = '*' then
      match List.filter is_number_adjacent numbers with
        | [{ value = value1; _ }; { value = value2; _ }] -> Some (value1 * value2)
        | _ -> None
  else None


let sum_of_gear_ratios ({symbols; numbers; _} : engine_schema) : int =
  let process s acc =
    match calc_gear_ratio s numbers with
      | None -> acc
      | Some ratio -> ratio + acc in
  SymbolSet.fold process symbols 0


let () =
  let filename = "../resources/input_03.txt" in
  let lines = In_channel.with_open_text filename In_channel.input_lines in
  let engine_schema = parse_engine_schematic lines in
  let result = sum_of_gear_ratios engine_schema in
  print_int result; print_newline ()
