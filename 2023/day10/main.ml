
type dim = X | Y
type dir = Pos | Neg
type move = { dim:dim; dir:dir }

module Vec = Helper.Vec2

let one_step = function
  | Pos -> 1
  | Neg -> -1

let r = function
  | Pos -> Neg
  | Neg -> Pos

let reflect m = { m with dir = r m.dir }

let vec_move m =
  let step = one_step m.dir in
  match m.dim with
  | Y -> Vec.{ zero with y = step }
  | X -> { Vec.zero with x = step }

type cell =
  | Empty
  | Edge of (move * move)
  | Start

let matching_move mv = function
  | Start | Empty -> false
  | Edge (x,y) ->
    let m' = reflect mv in
    x = m' || y = m'

let next_move (out1, out2) m =
  let m' = reflect m in
  if m' = out1 then out2
  else if m' = out2 then out1
  else assert false


let (.%()) m v = m.(v.Vec.x).(v.Vec.y)




let (.!()) m v = match m.%(v) with
  | Edge x -> x
  | Start | Empty -> assert false

let (.%()<-) m (v:Vec.t) x = m.(v.x).(v.y) <- x



let xp = { dim=X; dir = Pos }
let xm = { dim=X; dir = Neg }
let yp = { dim=Y; dir = Pos }
let ym = { dim=Y; dir = Neg }

let pp_move ppf = function
  | { dim = Y; dir = Neg } -> Format.fprintf ppf "←"
  | { dim = Y; dir = Pos } -> Format.fprintf ppf "→"
  | { dim = X; dir = Neg } -> Format.fprintf ppf "↑"
  | { dim = X; dir = Pos } -> Format.fprintf ppf "↓"


let rec loop steps mv pos visited map =
  let pos = Vec.( pos + vec_move mv) in
  if visited.%(pos) then steps
  else
    let cell = map.!(pos) in
    visited.%(pos) <- true;
    let mv = next_move cell mv in
    loop (steps+1) mv pos visited map

let parse_char = function
  | '-' -> Edge (ym, yp)
  | '|' -> Edge (xm, xp)
  | 'L' -> Edge (xm, yp)
  | 'J' -> Edge (xm, ym)
  | 'F' -> Edge (xp, yp)
  | '7' -> Edge (xp, ym)
  | 'S' -> Start
  | _ -> Empty

let pp_cell ppf = function
  | Start -> Format.fprintf ppf "S"
  | Empty -> Format.fprintf ppf " "
  | Edge ({ dim = X; dir = Neg  }, { dim=X; dir=Pos }) ->
          Format.fprintf ppf "│"
  | Edge ({ dim = Y; dir = Neg  }, { dim=Y; dir=Pos }) ->
          Format.fprintf ppf "─"
  | Edge ({ dim = X; dir = Neg  }, { dim=Y; dir=Pos }) ->
          Format.fprintf ppf "└"
  | Edge ({ dim = X; dir = Neg  }, { dim=Y; dir=Neg }) ->
          Format.fprintf ppf "┘"
  | Edge ({ dim = X; dir = Pos  }, { dim=Y; dir=Pos }) ->
          Format.fprintf ppf "┌"
  | Edge ({ dim = X; dir = Pos  }, { dim=Y; dir=Neg }) ->
          Format.fprintf ppf "┐"
  | _ -> assert false


let parse_line l =
  Array.init (String.length l) (fun i -> parse_char l.[i])

let grid data =
  Array.of_list @@ List.rev @@
  Helper.Input.fold (fun acc x ->
      parse_line x :: acc
    ) [] data



let sep ppf () = Format.fprintf ppf "@,"

let pp_line elt ppf x =
  Format.fprintf ppf "@[<h>%a@]"
  Helper.Pp.(seq ~sep elt) (Array.to_seq x)


let pp_grid elt ppf x =
  Format.fprintf ppf "@[<v>%a@]"
  Helper.Pp.(seq ~sep @@ pp_line elt) (Array.to_seq x)

let _transpose g =
  Array.init (Array.length g.(0)) (fun i ->
      Array.init (Array.length g) (fun j ->
          g.(j).(i)
        )
    )

type state =
  | Loop of move * move
  | Inside
  | Outside
  | Unknown

let pp_state ppf = function
  | Inside -> Format.fprintf ppf "*"
  | Outside -> Format.fprintf ppf " "
  | Unknown -> Format.fprintf ppf "?"
  | Loop (x,y)->  pp_cell ppf (Edge(x,y))


let mat_init m f =
  Array.init (Array.length m) (fun i ->
      Array.init (Array.length m.(i)) (fun j -> f i j)
    )

let init visited grid = mat_init grid (fun i j ->
    match visited.(i).(j), grid.(i).(j) with
    | false, _ -> Unknown
    | true, Start -> assert false
    | true, Empty -> assert false
    | true, Edge (x,y) -> Loop (x,y)
  )

let rec scan_line counter parity pos l =
  if pos >= Array.length l then counter
  else match l.(pos) with
    | Loop (x,y) ->
      if x = xm && y = xp then
        scan_line counter (not parity) (pos + 1) l
      else if (x.dim = X) then
        scan_end_of_border counter x.dir parity (pos+1) l
      else assert false
    | Inside | Outside -> assert false
    | Unknown ->
      if parity then
        (l.(pos) <- Inside; scan_line (1+counter) parity (pos+1) l)
      else
        (l.(pos) <- Outside; scan_line counter parity (pos+1) l)
and scan_end_of_border counter dir parity pos l =
  match l.(pos) with
  | Inside | Unknown | Outside -> assert false
  | Loop (x,y) ->
    if x.dim = X then
      let parity = if x.dir = dir then parity else not parity in
      scan_line counter parity (pos+1) l
    else begin
      assert (x.dim = Y && y.dim = Y);
      scan_end_of_border counter dir parity (pos+1) l
    end

let scan_grid g = Array.fold_left (fun acc l -> scan_line acc false 0 l) 0 g

let main data =
  let g = grid data in
  let start = Array.find_mapi (fun i a ->
      Array.find_mapi (fun j x -> if x = Start then Some(Vec.make i j) else None) a
    ) g
  in
  let () = Format.eprintf "%a@." (pp_grid pp_cell) g in
  let start = match start with
    | None -> assert false
    | Some s -> s
  in
  let start_dir mv =
    let pos = Vec.(start + vec_move mv) in
    if pos.x < 0 || pos.x >= Array.length g || pos.y < 0 || pos.y >= Array.length g.(0) then None else
    let cell = g.%(pos) in
    Format.eprintf "testing %a %a = %a@." Vec.pp pos pp_move mv pp_cell cell;
    if matching_move mv cell then Some mv else None
  in
  let dir = List.filter_map start_dir [xm;xp;ym;yp] in
  let dir = match List.sort compare dir with
    | [x;y] ->
      g.%(start)<- Edge (x, y);
      x
    | [] | [_] -> assert false
    | x :: _ -> Format.eprintf "Strange... @."; x
  in
  let visited = Array.init (Array.length g) (fun i ->
      Array.init (Array.length g.(i)) (Fun.const false)
    )
  in
  visited.%(start) <- true;
  let loop_size = loop 1 dir start visited g in
  Format.printf "loop radius=%d@." (loop_size/2);
  let init_area = init visited g in
  Format.eprintf "------------@.%a@." (pp_grid pp_state) init_area;
  let area = scan_grid init_area in
  Format.eprintf "----------------@.%a@." (pp_grid pp_state) init_area;
  Format.printf "Area=%d@." area

let () = main "2023/day10/data/input"
