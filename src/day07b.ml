type hand_type = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind
type hand = { cards : int list; type_ : hand_type }
type bid = { hand : hand; amount : int }

module IntMap = Map.Make(Int)


let is_digit = function
  | '0' .. '9' -> true
  | _ -> false


let int_of_card = function
  | 'A' -> 14
  | 'K' -> 13
  | 'Q' -> 12
  | 'T' -> 10
  | c when is_digit c -> int_of_string (String.make 1 c)
  | 'J' -> 1
  | _ -> failwith "Unknown card value."


let compare_hands ({cards = cards1; type_ = t1} : hand) ({cards = cards2; type_ = t2} : hand) : int =
  compare (t1, cards1) (t2, cards2)


let add_joker = function
    | FourOfAKind -> FiveOfAKind
    | ThreeOfAKind -> FourOfAKind
    | TwoPair -> FullHouse
    | OnePair -> ThreeOfAKind
    | HighCard -> OnePair
    | other -> other


let parse_hands (lines : string list) : bid list =
  let determine_type cards =
    let increment = function
      | None -> Some 1
      | Some count -> Some (count + 1) in
    let counts = cards |> List.fold_left (fun acc c -> IntMap.update c increment acc) IntMap.empty |> IntMap.bindings in
    let nr_jokers = Option.value (List.assoc_opt 1 counts) ~default:0 in
    let sorted_counts =
      List.sort (fun (_, cnt1) (_, cnt2) -> compare cnt2 cnt1) @@ List.filter (fun (v, _) -> v <> 1) counts in
    let initial_hand = match sorted_counts with
      | [(_, 5)] -> FiveOfAKind
      | (_, 4) :: _ -> FourOfAKind
      | [(_, 3); (_, 2)] -> FullHouse
      | (_, 3) :: _ -> ThreeOfAKind
      | (_, 2) :: (_, 2) :: _ -> TwoPair
      | (_, 2) :: _ -> OnePair
      | _ -> HighCard in
    List.fold_left (fun acc _ -> add_joker acc) initial_hand (List.init nr_jokers (fun _ -> 1)) in
  let parse_hand line =
    let cards, amount =
      Scanf.sscanf line "%c%c%c%c%c %d" (fun c1 c2 c3 c4 c5 d -> List.map int_of_card [c1; c2; c3; c4; c5], d) in
    {hand = {cards; type_ = determine_type cards}; amount} in

  List.map parse_hand lines


let calc_total_winnings (bids : bid list) : int =
  let nr_bids = List.length bids in
  let sorted_bids = List.sort (fun {hand = hand1; _} {hand = hand2; _} -> compare_hands hand1 hand2) bids in
  List.fold_left2 (fun acc {amount; _} k -> acc + amount * k) 0 sorted_bids (List.init nr_bids (fun k -> k + 1))


let () =
  let filename = "../resources/input_07.txt" in
  let lines = In_channel.with_open_text filename In_channel.input_lines in
  let bids = parse_hands lines in
  let result = calc_total_winnings bids in
  print_int result; print_newline ()
