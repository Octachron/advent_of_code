

let[@warning "-32"] parse_card_part_1 = function
  | '0' .. '9' as c -> Char.code c - Char.code '0'
  | 'T' -> 10
  | 'J' -> 11
  | 'Q' -> 13
  | 'K' -> 14
  | 'A' -> 15
  | _ -> assert false

let parse_card_part_2 = function
  | '0' .. '9' as c -> Char.code c - Char.code '0'
  | 'T' -> 10
  | 'Q' -> 13
  | 'K' -> 14
  | 'A' -> 15
  | 'J' -> -1
  | _ -> assert false


let parse_hand card h =
  Array.init (String.length h) (fun n -> card h.[n])

type step = S | O


type with_joker =
  | No_joker of  (step * step * step * step)
  | One_joker of (step * step * step)
  | Two_joker of (step * step)
  | Three_joker of step
  | Always_five

let normalized_hand h =
  let a = h |> Array.to_seq |> Seq.filter (fun n -> n >= 0) |> Array.of_seq in
  let () = Array.sort Stdlib.compare a in
  let steps = Array.make (Array.length a) O in
  for i = 1 to Array.length a - 1 do
    if a.(i) <> a.(i-1) then steps.(i) <- S
    done;
  match Array.length a with
  | 5 ->
    No_joker (steps.(1), steps.(2), steps.(3), steps.(4))
  | 4 -> One_joker (steps.(1), steps.(2), steps.(3))
  | 3 -> Two_joker (steps.(1), steps.(2))
  | 2 -> Three_joker steps.(1)
  | 1 | 0 -> Always_five
  | _ -> assert false

type hand =
  | High
  | Pair
  | Pairs
  | Three
  | Full
  | Four
  | Five


let score_no_joker = function
  | O, O, O, O -> Five
  | S, S, S, S -> High
  | O, O, O, S | S, O, O, O -> Four
  | O, O, S, O | O, S, O, O -> Full
  | O, O, S, S | S, O, O, S | S, S, O, O -> Three
  | O, S, O, S | S, O, S, O | O, S, S, O -> Pairs
  | O, S, S, S | S, O, S, S | S, S, O, S | S, S, S, O -> Pair

let score_one_joker = function
  | O, O, O -> Five
  | S, S, S -> Pair
  | O, O, S | S, O, O -> Four
  | O, S, O   -> Full
  | O, S, S | S, S, O | S, O, S-> Three

let score_two_joker = function
  | O, O -> Five
  | S, S -> Three
  | S, O | O, S -> Four


let score_three_joker = function
  | O -> Five
  | S -> Four

let score h =
  let k = match normalized_hand h with
    | No_joker h -> score_no_joker h
    | One_joker h -> score_one_joker h
    | Two_joker h -> score_two_joker h
    | Three_joker h -> score_three_joker h
    | Always_five -> Five
  in
  k, h

let compare_g (_, h) (_, h') =
    compare (score h) (score h')

let parse_line c l =
  Scanf.sscanf l "%s %d" (fun h b -> b, parse_hand c  h)

let games c data =
  List.rev @@ Helper.Input.fold (fun acc x -> parse_line c x ::  acc) [] data

let main c data =
  let games = games c data in
  let sort = List.sort compare_g games in
  let _, total_score =
    List.fold_left (fun (i,acc) (b,_) ->
        succ i, acc + b * i
      ) (1,0) sort
  in
  Format.eprintf "score=%d@." total_score

let () = main parse_card_part_2 "2023/day7/data/input"
