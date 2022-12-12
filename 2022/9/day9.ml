[@@@warning "-unused-value-declaration"]

module Vec = Helper.Vec2

let parse_dir = function
  | "U" | "u" -> Vec.make 0 1
  | "d" | "D" -> Vec.make 0 (-1)
  | "l" | "L" -> Vec.make (-1) 0
  | "r" | "R" -> Vec.make 1 0
  | s -> Helper.Pp.failf "Unknown direction:%s@." s

module Pos_set = Set.Make(Vec)

let normalize v =
  let clamp x =
    if x > 1 then 1
    else if x < -1 then -1
    else x
  in
  Vec.map clamp v

type state = { knots: Vec.t list; explored: Pos_set.t }

type move = Move of Vec.t | Same

let tail_move parent tail =
    if Vec.dist_inf parent tail > 1 then
      let move = normalize Vec.(parent - tail) in
      let tail = Vec.(tail + move) in
      Move tail
    else Same



let rec update ancestors parent knots explored =
  match knots with
  | [] ->
    let explored = Pos_set.add parent explored in
    { knots = List.rev (parent :: ancestors); explored }
  | x :: q ->
    match tail_move parent x with
    | Same ->
      let knots = List.rev (List.rev_append knots @@ parent :: ancestors) in
      { knots; explored }
    | Move x ->
      update (parent::ancestors) x q explored

let initial_update dir state = match state.knots with
  | [] -> assert false
  | start :: q ->
    let res = update [] Vec.(start + dir) q state.explored in
    res

let rec repeat k f x =
  if k = 0 then x else repeat (k-1) f (f x)

let f state line =
  let times, dir = Scanf.sscanf line "%s %d" (fun dir x -> x, parse_dir dir) in
  repeat times (initial_update dir) state

let () =
  let pos = { Vec.x=0; y=0 } in
  let start = { knots=List.init 10 (fun _ -> pos); explored = Pos_set.singleton pos } in
  let res = Helper.Input.fold f start "9/data/input"  in
  Format.printf "location explored=%d@." (Pos_set.cardinal res.explored)
