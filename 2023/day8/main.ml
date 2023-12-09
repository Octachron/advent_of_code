
type id = string
type dir = Left | Right
type edges = { left:id; right:id }

let (.!()) e = function
  | Left -> e.left
  | Right -> e.right

module Digraph = Map.Make(String)

module Node_set = Set.Make(String)

let (.%()) d k = Digraph.find k d


let parse_dir = function
  | 'L' -> Left
  | 'R' -> Right
  | _ -> assert false

let parse_dirs l =
  Array.init
    (String.length l )
    (fun i -> parse_dir l.[i])

let add_edge m x =
  Scanf.sscanf x "%s = (%s@, %s@)" (fun node left right ->
      Digraph.add node {left;right} m
    )

module Position = struct
  type t = { step:int; node:id }
  let compare = Stdlib.compare

  let next digraph dirs pos =
    let edge = digraph.%(pos.node) in
    let dir = dirs.(pos.step mod Array.length dirs) in
    let node = edge.!(dir) in
    let step = 1 + pos.step in
    {step; node}
end
open Position

module Pos_map = Map.Make(Position)


module[@warning "-32"] Parallel = struct
  type set_position = { node_set: Node_set.t; step: int }


  let next_set digraph dirs spos =
    let step = spos.step in
    let node_set = Node_set.map (fun node ->
        let pos' = Position.next digraph dirs { node; step } in
        pos'.node
      ) spos.node_set
    in
    Format.eprintf "@[{%a}@]@." Helper.Pp.(list ostring) (Node_set.elements node_set);
    { step = 1 + step; node_set }

  let stop_set x = Node_set.for_all (fun id -> id.[2] = 'Z') x.node_set

  let init_set g = Digraph.fold (fun k _ s ->
      if k.[2] = 'A' then Node_set.add k s else s
    ) g Node_set.empty
end

let normalize d s = { s with step = s.step mod d}

let end_point v =
  v |> Pos_map.to_seq |> Seq.filter_map (fun (k,x) ->
      if k.node.[2] = 'Z' then Some x else None)
  |> List.of_seq

let rec trajectory digraph dirs visited pos =
  let pos0 = normalize (Array.length dirs) pos in
  match Pos_map.find_opt pos0 visited with
  | Some time -> end_point visited, time, pos.step - time
  | None ->
    let visited = Pos_map.add pos0 pos.step visited in
    let pos = next digraph dirs pos in
    trajectory digraph dirs visited pos

let node_list g =
  g |> Digraph.to_seq |> Seq.filter_map (fun (k,_) ->
      if k.[2] = 'A' then Some k else None
    )
  |> List.of_seq

let parse data =
  let _, dirs, digraph = Helper.Input.fold (fun (i, dirs, g) l ->
      match i with
      | 0 -> 1 + i, parse_dirs l, g
      | 1 -> 1 + i, dirs, g
      | _ ->
        1 + i, dirs, add_edge g l
    ) (0,[||],Digraph.empty) data
  in
  dirs, digraph

let[@warning "-32"] main_part_1 data =
  let dirs, g = parse data in
  let pos = { step=0; node = "AAA" } in
  let seq = Seq.iterate (next g dirs) pos in
  match Seq.find (fun p -> p.node ="ZZZ") seq with
  | None -> assert false
  | Some pos ->
    Format.printf "ZZZ found in %d steps@." pos.step

let rec pgcd x y =
  if y = 0 then x
  else let r = x mod y in
    pgcd y r

let ppm l = List.fold_left (fun x y ->
    x * y / pgcd x y
  ) 1 l

let main_part_2 data =
  let dirs, g = parse data in
  let node_set = List.map (fun node -> {node;step=0}) @@ node_list g in
  Format.eprintf "Number of directions: %d@." (Array.length dirs);
  Format.eprintf "Graph size: %d@." (Digraph.cardinal g);
  let trajectories = List.map (trajectory g dirs Pos_map.empty) node_set in
  let pp_ends = Helper.Pp.(list int) in
  let end_points = List.map (fun (l,_,_) -> match l with
      | [x] -> x
      | _ -> assert false
    ) trajectories
  in
  let cycle = ppm end_points in
  let pp_t ppf (points,start, len) =
    Format.fprintf ppf "Cycle at %d, period:%d, ends at @[[%a]@]"
      start len pp_ends points
  in
  Format.printf "@[<v>%a@]@." (Helper.Pp.list pp_t) trajectories;
  Format.printf "full period=%d@." cycle


let () = main_part_2 "2023/day8/data/input"
