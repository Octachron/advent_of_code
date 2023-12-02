type sample = { red:int; blue:int; green:int }

type game = { id: int; samples: sample list }
let zero = { green = 0; red = 0; blue = 0 }

let join x y = {
  green=max x.green y.green;
  red = max x.red y.red ;
  blue = max x.blue y.blue
}

let add_color g c =
  Scanf.sscanf c " %d %s" (fun n c ->
      match c with
      | "green" -> { g with green = n }
      | "red" -> { g with red = n }
      | "blue" ->  { g with blue = n }
      | _ -> assert false
    )


let pp_game ppf g = Format.fprintf ppf "{red=%d; green=%d; blue=%d}" g.red g.green g.blue


let (<<=) x y =
  x.green <=  y.green && x.red <= y.red && x.blue <= y.blue

let ref_game = { red=12; green=13; blue=14 }

let parse_game g =
  g
  |> String.split_on_char ','
  |> List.fold_left add_color zero

let parse_line line =
  let id, rest = match String.split_on_char ':' line with
  | [x;y] ->
    Scanf.sscanf x "Game %d" Fun.id, y
  | _ -> assert false
  in
  let games =
    let games = String.split_on_char ';' rest in
    List.map parse_game games
  in
  { id; samples= games }

module[@warning "-32"] Part_1 = struct
  let add_possible_id score game =
    if List.for_all (fun g ->  (g <<= ref_game) ) game.samples then begin
      Format.eprintf "@[<b 2>game %d=@ @[<b 2>[%a]@]@]@." game.id (Helper.Pp.list pp_game) game.samples;
      game.id + score
    end
    else
      score

  let read f = Helper.Input.fold (fun score line ->
      line |> parse_line |> add_possible_id score
    )
      0 f

end

module Part_2 = struct
  let power game =
    let join = List.fold_left join zero game.samples in
    join.green * join.red * join.blue

  let read f = Helper.Input.fold (fun score line ->
      let line_score = line |> parse_line |> power in
      score + line_score
    )
    0 f

  let main () =
    let sum = read "2023/day2/data/input" in
    Format.printf "@[<v>@,power sum=%d@]@." sum

end

let () = Part_2.main ()
