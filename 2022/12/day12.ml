module Vec = Helper.Vec2

module Min_map = Map.Make(Int)
module Vset = Set.Make(Vec)
module Vmap = Map.Make(Vec)

type path_partial = { steps: int; path: Vset.t}

type path_info = { steps:int; pos:Vec.t; path: Vset.t }



let extend pos (x:path_partial): path_info =
  { steps = x.steps; path = x.path; pos }
let partial (x:path_info): path_partial =
   {steps=x.steps; path=x.path }

let best_candidate s =
  let step, nodes = Min_map.choose s in
  let pos, info = Vmap.choose nodes in
  let nodes = Vmap.remove pos nodes in
  let s =
    if Vmap.is_empty nodes then
      Min_map.remove step s
    else
      Min_map.add step nodes s
  in
  step, extend pos info , s

let add_candidate step path candidates =
  match Min_map.find_opt step candidates with
  | None ->
    Min_map.add step (Vmap.singleton path.pos (partial path) ) candidates
  | Some nodes ->
    let nodes =
      match Vmap.find_opt path.pos nodes with
      | None ->
        Vmap.add path.pos (partial path) nodes
      | Some q ->
        if q.steps <= path.steps then nodes
        else (assert false)
     in
    Min_map.add step nodes candidates


let (.!()) grid v =
  grid.(v.Vec.x).(v.Vec.y)


let (.!()<-) grid v x =
  grid.(v.Vec.x).(v.Vec.y)<-x


let moves = Vec.[ make 0 1; make 0 (-1); make 1 0; make (-1) 0]

type map = {
  goal:Vec.t;
  cond: before:int -> after:int -> bool;
  eq: Vec.t -> Vec.t -> bool;
  grid: int array array;
  visited: bool array array;
  dist: Vec.t -> Vec.t -> int
}


let add_new_candidate {dist;goal; _ } candidates path =
    let optimistic_dist = path.steps + dist goal path.pos in
    add_candidate optimistic_dist path candidates


let inside {Vec.x; y} grid =
  let lx = Array.length grid in
  let ly = Array.length grid.(0) in
  x >= 0 && x < lx && y >= 0 && y < ly

let move_to {cond;visited;grid;_} path dir =
  let pos = Vec.(path.pos + dir) in
  if not (inside pos grid) then []
  else if visited.!(pos) then []
  else
    let alt = grid.!(pos) in
    if cond ~before:grid.!(path.pos) ~after:alt then
      [{ pos; steps = path.steps + 1; path = Vset.add pos path.path}]
    else
      []

let pp_visited ~with_reset ppf (path,map) =
  let lx = Array.length map.grid in
  let ly = Array.length map.grid.(0) in
  Format.fprintf ppf "@[";
  for i = 0 to lx - 1 do
    for j = 0 to ly - 1 do
      let c = Char.chr (map.grid.(i).(j) + Char.code 'a') in
      if Vset.mem (Vec.make i j) path then
        Format.fprintf ppf "\027[32m%c" c
      else if map.visited.(i).(j) then
        Format.fprintf ppf "\027[31m%c" c
      else
        Format.fprintf ppf "\027[39m%c" c
    done;
    Format.fprintf ppf "@,"
  done;
  if with_reset then (for _i = 0 to lx - 1 do
    Format.fprintf ppf "\027[1A";
  done);
  Format.fprintf ppf "@]"


let rec shortest_path map candidates =
  let _score, path, candidates = best_candidate candidates in
  if map.eq path.pos map.goal then Some path else
  let () = map.visited.!(path.pos) <- true in
  let new_candidates = List.concat_map (move_to map path) moves in
  let candidates =
    List.fold_left (add_new_candidate map) candidates new_candidates
  in
    if Min_map.cardinal candidates = 0 then
      None
    else
      shortest_path map candidates

let _map_part_1 goal grid =
  let alt_diff x y =
    let diff = grid.!(y) - grid.!(x) in
    if diff < 0 then 0 else diff
  in
  let eq x y = x = y in
  let cond ~before ~after = after - before <= 1 in
  let dist x y = max (Vec.dist_1 x y) (alt_diff x y) in
  let visited = Array.make_matrix (Array.length grid) (Array.length grid.(0)) false in
  { goal; grid; visited; dist; cond; eq }

let map_part_2 goal grid =
  let alt_diff x y = abs ( grid.!(y) - grid.!(x)) in
  let cond ~before ~after = after - before >= -1 in
  let eq x y = grid.!(x) = grid.!(y) in
  let dist x y = alt_diff x y in
  let visited = Array.make_matrix (Array.length grid) (Array.length grid.(0)) false in
  { goal; grid; visited; dist; eq; cond}



let parse_alt = function
  | 'S' -> 0
  | 'a' .. 'z' as x -> Char.code x - Char.code 'a'
  | 'E' -> Char.code 'z' - Char.code 'a'
  | c -> Helper.Pp.failf "unknown altitude:%c" c

let parse_line (i, start, emit, lines) s =
  let find_char v c = match v, String.index_opt s c with
    | Some _, Some _ -> Helper.Pp.failf "Two %c?" c
    | Some _ as s, None -> s
    | None, Some j -> Some (Vec.make i j)
    | None, None -> None
  in
  let start = find_char start 'S' in
  let emit = find_char emit 'E' in
  let i = i + 1 in
  let a = Array.init (String.length s) (fun n -> parse_alt s.[n]) in
  (i, start, emit, a :: lines)




let () =
  let _, start, emit, rev_lines =
    Helper.Input.fold parse_line (0,None,None,[]) "12/data/input"
  in
  let start, emit = match start, emit with
    | Some start, Some emit -> start, emit
    | _ -> assert false
  in
  let grid = Array.of_list @@ List.rev rev_lines in
  let map = map_part_2 start grid in
  let pos = emit in
  let init_path = { pos; steps = 0;  path = Vset.singleton pos} in
  let res = shortest_path map (Min_map.singleton 0 (Vmap.singleton pos (partial init_path))) in
  match res with
  | Some p ->
    Format.printf "@.%a@.number of steps=%d@." (pp_visited ~with_reset:false) (p.path,map) p.steps
  |  None ->
    assert false
