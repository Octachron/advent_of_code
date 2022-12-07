module Pp = Helper.Pp

type 'a entry =
  | Dir of 'a
  | File of int


type dir = { size:int: name:string; children: dir entry list}



type cursor = { left: dir list; file_size: int; right: string list }
type 'a zipper = { name:string; focus: 'a; path:cursor list}

let up z = match z.focus, z.path with
  | c, [] -> state
  | { right = _ :: _ ; _ }, x :: q -> Pp.failf "Directory with unknown size left"
  | { right = []; left; current; file_size } :: q ->
    let size = file_size + List.fold_left (fun x d -> x + d.size ) 0 left in
    let dir = { size; name=current; children = left } in
    { x with left = dir :: x.left }:: q

let right name z =
  match List.partition (fun (s,_x)-> s = name) z.focus.right with
  | [] , _ -> Pp.failf "Unknown directory %s" name
  | _ :: _ :: _ , _ -> Pp.failf "Too many directory called %s" name
  | [d], right ->
    let parent = { z.focus with right }
    { name; focus = (); path = parent :: z.path }
