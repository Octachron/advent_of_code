let fold f start filename =
  let rec loop f acc ch =
      match In_channel.input_line ch with
      | Some x -> loop f (f acc x) ch
      | None -> acc
  in
  In_channel.with_open_bin filename (fun ch -> loop f start ch)

type play =
  | Rock
  | Paper
  | Scissor

type player =
  | X
  | Y
  | Z

let failf fmt = Format.kfprintf (fun ppf -> Format.fprintf ppf "@."; exit 2)
    Format.err_formatter fmt

let parse_p1 = function
  | "A" -> Rock
  | "B" -> Paper
  | "C" -> Scissor
  | s -> failf "Unknown elf input: %s" s

let parse_p2 = function
  | "X" -> X
  | "Y" -> Y
  | "Z" -> Z
  | s -> failf "Unknown human input: %s" s

let _simple_interpretation (_x,y) =
  match y with
  | X -> Rock
  | Y -> Paper
  | Z -> Scissor

type game_issue =
  | Win
  | Draw
  | Lose

let win_interpretation = function
  | X -> Lose
  | Y -> Draw
  | Z -> Win

let state_interpretation (x,y) = match x, win_interpretation y with
  | Rock, Win | Paper, Draw | Scissor, Lose -> Paper
  | Paper, Win | Scissor, Draw | Rock, Lose -> Scissor
  | Scissor, Win | Rock, Draw | Paper, Lose -> Rock

let line s = Scanf.sscanf s "%s %s" (fun x y -> parse_p1 x, parse_p2 y)

let score_act (_,y) = match y with
  | Rock -> 1
  | Paper -> 2
  | Scissor -> 3

let score_game_2 = function
  | Paper, Paper | Rock, Rock | Scissor, Scissor -> Draw
  | Rock, Paper | Paper, Scissor | Scissor, Rock -> Win
  | Paper, Rock | Scissor, Paper | Rock, Scissor -> Lose

let score_point = function
  | Win -> 6
  | Draw -> 3
  | Lose -> 0

let score_all eh = score_point (score_game_2 eh) + score_act eh

let score_strategy strategy s =
  let add_score_line x s =
    let elf, _ as line = line s in
    x + score_all (elf, strategy line)
  in
  fold add_score_line 0 s

let () =
  Format.printf "score=%d@." (score_strategy state_interpretation "2/data/input")
