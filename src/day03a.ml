open Batteries

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


let read_lines (filename : string) : string list =
  In_channel.with_open_text filename (In_channel.input_lines)


let parse_single_line (line : string) (row_index : int) : symbol list * number list =
  let rec loop symbols numbers column_ix = function
    | [] -> (symbols, numbers)
    | (c :: css) as cs ->
      if c = '.' then loop symbols numbers (column_ix + 1) css
      else if Char.is_digit c then
        let (digits, rest) = List.span Char.is_digit cs in
        let length = List.length digits in
        let start = { x = row_index; y = column_ix } in
        let n = { start; length; value = int_of_string @@ String.concat "" (List.map (String.make 1) digits) } in
        loop symbols (n :: numbers) (column_ix + length) rest
      else
        let position = { x = row_index; y = column_ix } in
        loop ({ position; value = c } :: symbols) numbers (column_ix + 1) css in
  loop [] [] 0 (String.to_list line)


let parse_engine_schematic (lines : string list) : engine_schema =
  let rec loop symbols numbers row_ix = function
    | [] -> { symbols; numbers = List.rev numbers }
    | line :: ls ->
      let syms, ns = parse_single_line line row_ix in
      loop (SymbolSet.union symbols (SymbolSet.of_list syms)) (ns @ numbers) (row_ix + 1) ls in
  loop SymbolSet.empty [] 0 lines


let adjacent_to_any_symbol ({symbols; _} : engine_schema) ({start = {x; y}; length; _} : number) : bool =
  let is_symbol_adjacent {position = {x = sx; y = sy}; _} =
    abs (sx - x) <= 1 && y - 1 <= sy && sy <= y + length in
  SymbolSet.exists is_symbol_adjacent symbols


let sum_of_part_numbers (schema : engine_schema) : int =
  List.fold_left (fun acc n -> if adjacent_to_any_symbol schema n then acc + n.value else acc) 0 schema.numbers


let () =
  let lines = read_lines "../resources/input_03.txt" in
  let engine_schema = parse_engine_schematic lines in
  let result = sum_of_part_numbers engine_schema in
  print_int result; print_newline ()
