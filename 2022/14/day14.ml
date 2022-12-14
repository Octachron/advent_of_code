module Vec = Helper.Vec2

type t = Vec.t = {x:int; y:int}
module G = Helper.Grid

let parse_line s =
  let lexbuf = Lexing.from_string s in
  Parser.line Lexer.main lexbuf

let rec interpolate_dir d x y () =
  if x = y then
    Seq.return x ()
  else
    Seq.cons x (interpolate_dir d Vec.(x+d) y) ()

let interpolate v w =
  match v.x = w.x, v.y = w.y with
  | true, false ->
    let start, stop =
      if v.y <= w.y then v, w
      else w, v
    in
    interpolate_dir (Vec.make 0 1) start stop
  | false, true ->
    let start, stop =
      if v.x <= w.x then v, w
      else w, v
    in
    interpolate_dir (Vec.make 1 0) start stop
  | true, true | false, false -> assert false

type cell =
  | Block
  | Empty
  | Sand

let draw grid start stop =
  Seq.iter (fun v -> grid.G.!(v) <- Block)
  @@ interpolate start stop

let rec draw_many grid start = function
  | a :: q ->
    draw grid start a;
    draw_many grid a q
  | [] -> ()

let draw_line grid l =
  match l with
  | a :: ( _ :: _ as q) -> draw_many grid a q
  | [_] | [] -> assert false

type result =
  | Bottom of Vec.t
  | Blocked of Vec.t

let rec add_sand grid start =
  let open G in
  let down = Vec.(start + make 0 1) in
  let down_right = Vec.(start + make 1 1) in
  let down_left = Vec.(start + make (-1) 1) in
  if start.y + 1 = grid.dims.y then Bottom start else
  match grid.!(down), grid.!(down_left), grid.!(down_right) with
    | Empty, _, _ -> add_sand grid down
    | (Sand|Block), Empty, _ -> add_sand grid down_left
    | (Sand|Block), (Sand|Block), Empty -> add_sand grid down_right
    | (Sand|Block), (Sand|Block),  (Sand|Block) ->
      Blocked start

let rec first_fall grid start n =
  match add_sand grid start with
  | Bottom _ -> n
  | Blocked v ->
    let () = grid.G.!(v) <- Sand in
    first_fall grid start (n+1)

let _ = first_fall

let rec fill_all grid start n =
  match add_sand grid start with
  | Bottom v ->
    let () = grid.G.!(v) <- Sand in
    fill_all grid start n + 1
  | Blocked v ->
    if v.y = 0 then (1+n)
    else
      let () = grid.G.!(v) <- Sand in
      fill_all grid start (n+1)


let _pp_block ppf = function
  | Empty -> ()
  | Block -> Format.fprintf ppf "#"
  | Sand -> Format.fprintf ppf "o"

let () =
  let lines = Helper.Input.fold (fun acc l -> parse_line l :: acc ) [] "14/data/input" in
  let max_y =
    Seq.fold_left (fun y v -> max y v.Vec.y) 0
    @@ Seq.concat @@ Seq.map List.to_seq @@ List.to_seq  lines
  in
  let grid = G.make (Vec.make 1000 (2 + max_y)) Empty in
  let () = List.iter (draw_line grid) lines in
  let d = fill_all grid (Vec.make 500 0) 0 in
  Format.printf "Fill all %d@." d
