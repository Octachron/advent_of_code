module V = Helper.Vec2

let _dist v w =
  let d = V.( v - w ) in
  V.dist_1 v w - min (abs d.x) (abs d.y)

let cumulative a =
  let a = Array.copy a in
  for i = 1 to Array.length a - 1 do
    a.(i) <- a.(i) + a.(i-1)
  done;
  a

module Int_set = Set.Make(Int)

let rec scan_line c (_lsize, ln,columns,lines,galaxies as st) line =
  let lsize = String.length line in
  if c >= lsize then (lsize, 1 + ln, columns, lines, galaxies)
  else match line.[c] with
    | '#' ->
      let g = V.make ln c in
      let columns = Int_set.add c columns in
      let lines = Int_set.add ln lines in
      scan_line (c+1) (lsize,ln, columns, lines, g :: galaxies) line
    | '.' ->
      scan_line (1+c) st line
    | _ -> assert false

let parse data =
  Helper.Input.fold (scan_line 0)
    (0, 0, Int_set.empty, Int_set.empty, [])
    data

let dilatation ~factor n set =
  let a = Array.init n (fun k -> if Int_set.mem k set then 1 else factor) in
  cumulative a


type map = { columns: int array; lines: int array }

let translate m v = V.make m.lines.(v.V.x) m.columns.(v.V.y)
let dist m v w = V.dist_1 (translate m v) (translate m w)

let (let*) x f = List.concat_map f x

let pairs x =
  let* a = x in
  let* b = x in
  if a < b then [a,b] else []

module Pp = Helper.Pp

let main data =
  let csize, lsize, columns, lines, galaxies = parse data in
  Format.eprintf "lines: @[%a@]@." Pp.(seq int) (Int_set.to_seq lines);
  Format.eprintf "columns: @[%a@]@." Pp.(seq int) (Int_set.to_seq columns);
  Format.eprintf "galaxies:@[%a@]@." Pp.(list V.pp) galaxies;
  let factor = 1_000_000 in
  let columns = dilatation ~factor csize columns in
  let lines = dilatation ~factor lsize lines in
  Format.eprintf "column map:@[%a@]@." Pp.(seq int) (Array.to_seq columns);
  Format.eprintf "line map:@[%a@]@." Pp.(seq int) (Array.to_seq lines);
  let pairs = pairs galaxies in
  let maps = { lines; columns } in
  let score = List.fold_left (fun acc (x,y) ->
      let d = dist maps x y in
      acc + d
    ) 0 pairs in

  Format.printf "Distances= %d@." score

let () = main "2023/day11/data/input"
