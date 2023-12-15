
type state = { pos:int; last_empty_pos:int; load:int * int }

type cell =
  | Empty
  | Fixed
  | Rock

let update_score score x =
  let pos = score.pos + 1 in
  let score = { score with pos } in
  match x with
  | Empty -> score
  | Fixed -> { score with last_empty_pos = pos }
  | Rock ->
    let c, partial = score.load in
    let load = 1 + c, partial - score.last_empty_pos in
    let last_empty_pos = score.last_empty_pos + 1 in
    { load; pos; last_empty_pos }




let zero = { pos = 0; last_empty_pos=0; load = 0, 0 }

let parse_char = function
  | '.' -> Empty
  | '#' -> Fixed
  | 'O' -> Rock
  | _ -> assert false

let parse_line l = Array.init (String.length l) (fun i -> parse_char l.[i])
module Part_1 = struct
  let[@warning "-32"] main data =
    let score_array =
      Helper.Input.fold (fun scores l ->
          let scores = if scores = [||] then Array.make (String.length l) zero else scores in
          Array.map2 update_score scores (parse_line l)
        ) [||] data
    in
    let score = Array.fold_left (fun acc score ->
        let c, partial = score.load in
        acc + c*score.pos + partial)
        0 score_array
    in
    Format.printf "score = %d@." score
end

module Part_2 = struct

  module V = Helper.Vec2
  let shape g = V.make (Array.length g) (Array.length g.(0))


  let (.%()) m v = V.( m.(v.x).(v.y) )
  let (.%()<-) m v x = let open V in m.(v.x).(v.y) <- x
  let (<<) v w = let open V in  v.x < w.x && v.y < w.y
  let (>=>) v w = let open V in  v.x >= w.x && v.y >= w.y


let rec update_dir dir g last pos =
  if not (pos << shape g && pos >=> V.zero) then ()
  else
    let new_pos = V.(pos + dir) in
    match g.%(pos) with
    | Fixed -> update_dir dir g new_pos new_pos
    | Empty -> update_dir dir g last new_pos
    | Rock ->
      g.%(pos) <- Empty;
      g.%(last) <- Rock;
      let last = V.(last + dir) in
      update_dir dir g last new_pos

let rec update_grid ortho dir pos g =
  if not (pos << shape g && pos >=> V.zero) then ()
  else begin
    update_dir dir g pos pos;
    update_grid ortho dir V.(pos + ortho) g
  end


let update_north g =
  update_grid
    (V.make 0 1)
    (V.make 1 0)
    (V.make 0 0)
    g

let update_south g =
  update_grid
    (V.make 0 1)
    (V.make (-1) 0)
    (V.make (Array.length g - 1) 0)
    g

let update_west g =
  update_grid
    (V.make 1 0)
    (V.make 0 1)
    (V.make 0 0)
    g

let update_east g =
  update_grid
    (V.make 1 0)
    (V.make 0 (-1))
    (V.make 0 (Array.length g.(0) - 1))
    g

let pp_cell ppf = function
  | Empty -> Format.fprintf ppf "."
  | Fixed -> Format.fprintf ppf "#"
  | Rock -> Format.fprintf ppf "O"

let pp_mat_line ppf l = Array.iter (pp_cell ppf) l
let[@warning "-32"] pp_mat ppf a =
  Format.fprintf ppf "@[<v>%a@]"
    Helper.Pp.(seq pp_mat_line) (Array.to_seq a)

let cycle g =
  update_north g;
  update_west g;
  update_south g;
  update_east g


let rec range f s a pos =
  if pos >= Array.length a then s
  else range f (f s pos) a (1+pos)

let range f s a = range f s a 0

let score g =
  let rec column j s i =
    if i >= Array.length g then s
    else
      let s =
        match g.(i).(j) with
        | Rock -> s + Array.length g - i
        | Fixed | Empty -> s
      in
      column j s (1+i)
  in
  range (fun s j ->
      column j s 0
    ) 0 g.(0)


module Grid_map = Map.Make(struct type t = cell array array let compare = compare end)

let matrix_copy g = Array.map Array.copy g
[@@@warning "-32"]
let rec decompose map i g =
  match Grid_map.find_opt g map with
  | None ->
    let map = Grid_map.add (matrix_copy g) i map in
    cycle g;
    decompose map (1+i) g
  | Some time ->
    time, i - time, g

let parse data =
  Helper.Input.fold (fun acc l -> parse_line l :: acc)  [] data |> List.rev |> Array.of_list

let part_1_bis data =
  let grid = parse data in
  update_north grid;
  Format.eprintf "score = %d@." (score grid)

let part_2 data =
  let grid = parse data in
  let start, period, _ = decompose Grid_map.empty 0 grid in
  let n_iter = 1_000_000_000 in
  let rem = (n_iter - start) mod period in
  for _i=1 to rem do cycle grid done;
  Format.eprintf "score = %d@." (score grid)



end

let () = Part_2.part_2 "2023/day14/data/input"
