let parse_digit = function
  | '0' .. '9' as c -> Char.code c - Char.code '0'
  | _ -> assert false


module G = Helper.Grid
module V = Helper.Vec2
module D = G.Dir

type state = { dir:D.t; len: int }

let dir_index = function
  | D.Up -> 0
  | Down -> 1
  | Left -> 2
  | Right -> 3

let state_index s = dir_index s.dir + 4 * s.len



type param = { max_dir:int; min_dir:int }

let last param = { dir = D.Right; len = param.max_dir }

let state_init param  = Array.make (1 + state_index (last param)) (Int.max_int, [])

let reflect = function
  | D.Left -> D.Right
  | Up -> Down
  | Down -> Up
  | Right -> Left

let move {max_dir; min_dir} grid (pos,inertia) dir =
  let pos = V.( pos + D.vec2 dir ) in
  if not (G.in_grid grid pos) then []
  else if inertia.dir = dir then begin
    if 1 + inertia.len = max_dir then []
    else [pos, { inertia with len = 1 + inertia.len }]
  end
  else if inertia.dir = reflect dir then []
  else if 1 + inertia.len >= min_dir then [pos, { dir; len = 0 } ]
  else []

let all_dir = [ D.Up; Left; Right; Down ]



let pp_exponent ppf = function
  | 0 -> ()
  | 1 -> Format.fprintf ppf "²"
  | 2 -> Format.fprintf ppf "³"
  | 3 -> Format.fprintf ppf "4"
  | d -> Format.fprintf ppf "%d" (1+d)

let pp_pos ppf (v,i) = Format.fprintf ppf "%a(%a%a)" V.pp v D.pp i.dir pp_exponent i.len

let neighbor param grid ipos =
  let moves =  List.concat_map (move param grid ipos) all_dir in
  moves


let (.!()) graph (pos:V.t) = graph.(pos.x).(pos.y)


let (.%()) graph (pos,inertia) =
  let a = graph.!(pos) in
  if inertia.len < 0 then Int.max_int, []
  else a.(state_index inertia)

let (.%()<-) graph (pos,inertia) x = graph.!(pos).(state_index inertia) <- x


module Prio = Map.Make(struct
    type t = int
    let compare = Stdlib.compare
end)

let add heat ipos q =
  match Prio.find_opt heat q with
  | None -> Prio.add heat [ipos] q
  | Some l -> Prio.add heat (ipos::l) q


let visit cost_map graph heat history queue ipos =
  let heat = heat + cost_map.!(fst ipos) in
  if heat < fst graph.%(ipos) then begin
    graph.%(ipos) <- (heat, ipos::history);
    add heat ipos queue
  end else begin
    queue
  end


type ('a,'b) r =
  | Done of 'a
  | Keep_on of 'b

let rec fold f start = function
  | [] -> Keep_on start
  | a :: q ->
    match f start a with
    | Done _ as x -> x
    | Keep_on b -> fold f b q

      let keep_on x = Keep_on x

let extend_point param term cost_map graph heat queue p =
  if term p then Done p else
   let _h, history = graph.%(p) in
   keep_on @@ List.fold_left (visit cost_map graph heat history) queue (neighbor param cost_map p)


let rec shortest_path param term cost_map graph queue =
  if Prio.is_empty queue then begin Format.eprintf "No solutions?@."; exit 2 end else
  let heat, nexts = Prio.choose queue in
  let queue = Prio.remove heat queue in
  match fold (extend_point param term cost_map graph heat) queue nexts with
  | Done p -> p
  | Keep_on queue ->
    shortest_path param term cost_map graph queue


let term _param g ((pos:V.t), _inertia) =
  let gl = Array.length g - 1 in
  pos.x = gl  && pos.y = Array.length g.(gl) - 1

let main data =
  let cost_map = G.parse parse_digit data in
  let start = V.make 0 0 in
  let queue = Prio.singleton 0 [ start, {dir=Down; len= (-1)}; start, { dir=Right; len = (-1) } ] in
  let part_2_param = { min_dir = 4; max_dir = 10 } in
  let graph = Array.map (Array.map (fun _ -> state_init part_2_param)) cost_map in
  let p = shortest_path part_2_param (term part_2_param cost_map) cost_map graph queue in
  let heat, history = graph.%(p) in
  Format.printf "Path=@[<v>%a@]@." (Helper.Pp.list pp_pos) (List.rev history);
  Format.printf "Final heat lost at %a =%d@." pp_pos p heat


let () = main "2023/day17/data/input"
