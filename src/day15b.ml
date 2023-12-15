module List = struct
  include List

  let remove_first p xs =
    let rec loop acc = function
      | [] -> List.rev acc
      | x :: xss ->
          if p x then (List.rev acc) @ xss
          else loop (x :: acc) xss in
    loop [] xs

  let replace_with p f xs =
    let rec loop acc = function
      | [] -> List.rev acc
      | x :: xss ->
          if p x then loop ((f x) :: acc) xss
          else loop (x :: acc) xss in
    loop [] xs
end

type lens = { label : string; focal_length : int }

type operation =
  | Remove of { label : string }
  | Replace of lens


module HashMap : sig
  type t
  val empty : t
  val fold_lefti : ('acc -> int -> lens list -> 'acc) -> 'acc -> t -> 'acc
  val remove : string -> t -> unit
  val replace : lens -> t -> unit

end = struct
  type t = lens list array

  let p = 17
  let size = 256

  let hash_algorithm (str : string) : int =
    String.fold_left (fun acc chr -> p * (acc + Char.code chr) mod size) 0 str

  let empty = Array.make size []

  let fold_lefti f init arr =
    Seq.fold_lefti f init (Array.to_seq arr)

  let remove lbl arr =
    let ix = hash_algorithm lbl in
    let items = arr.(ix) in
    arr.(ix) <- List.remove_first (fun {label; _ } -> label = lbl) items

  let replace ({label; _} as lens) arr =
    let ix = hash_algorithm label in
    let items = arr.(ix) in
    let items' =
      match List.find_opt (fun {label = lbl; _} -> lbl = label) items with
        | None -> lens :: items
        | _ -> List.replace_with (fun ln -> ln.label = label) (fun _ -> lens) items in
    arr.(ix) <- items'
end


let operation_of_string (str : string) : operation =
  if String.ends_with ~suffix:"-" str then Remove { label = String.sub str 0 (String.length str - 1) }
  else
    match String.split_on_char '=' str with
      [label; fl] -> Replace { label = label; focal_length = int_of_string fl }
      | _ -> failwith "Malformed input."


let parse_operations (data : string) : operation list =
  data
    |> String.to_seq
    |> Seq.filter (fun c -> c <> '\n')
    |> String.of_seq
    |> String.split_on_char ','
    |> List.map operation_of_string


let calc_sum_of_focusing_powers (steps : operation list) : int =
  let calc_focusing_powers acc ix lenses =
    let n = List.length lenses in
    Seq.fold_lefti (fun s slot {focal_length; _} -> s + (ix + 1)*(n - slot)*focal_length) acc (List.to_seq lenses) in
  let hmap = HashMap.empty in
  let process = function
    | Remove {label} -> HashMap.remove label hmap
    | Replace lens -> HashMap.replace lens hmap in
  List.iter process steps;
  HashMap.fold_lefti calc_focusing_powers 0 hmap


let () =
  let filename = "../resources/input_15.txt" in
  let data = In_channel.with_open_text filename In_channel.input_all in
  let steps = parse_operations data in
  let result = calc_sum_of_focusing_powers steps in
  print_int result; print_newline ()
