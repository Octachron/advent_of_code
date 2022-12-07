[@@@warning "-37-34-32"]
module Pp = Helper.Pp

type entry =
  | Dir of string
  | File of int

type dir = { size:int; size_below_threshold:int; name:string}

let add_threshold th sum x = if x > th then sum else sum + x


type cursor = { left: dir list; size_below_threshold:int; file_size: int; right: string list }
type 'a zipper = { dirpath: string list; name:string; focus: 'a; path:cursor list}


let string ppf = Format.fprintf ppf "%s"
let slash ppf  () = Format.fprintf ppf "/"

let up (th:int) z = match z.focus, z.path with
  | _, [] -> z
  | { right = _ :: _ ; _ }, _x :: _q -> Pp.failf "Directory with unknown size left"
  | { right = []; left; file_size; _ },  parent :: path ->
    let size = file_size + List.fold_left (fun sum d -> sum + d.size) 0 left in
    let size_below_threshold =
      add_threshold th
        (List.fold_left (fun sum (d:dir) -> sum + d.size_below_threshold) 0 left)
        size
    in
    Format.printf "@[dir=%a size=%d, size_below=%d@]@." (Pp.list ~sep:slash string) (List.rev z.dirpath) size size_below_threshold;
    let dir = { size; name=z.name; size_below_threshold } in
    { focus = { parent with left = dir :: parent.left; size_below_threshold };
      dirpath = List.tl z.dirpath;
      name = "*"; path }

let right name focus z =
  match List.partition (fun s-> s = name) z.focus.right with
  | [] , _ -> Pp.failf "Unknown directory %s" name
  | _ :: _ :: _ , _ -> Pp.failf "Too many directory called %s" name
  | [_], right ->
    let parent = { z.focus with right } in
    { name; dirpath = name :: z.dirpath;  focus; path = parent :: z.path }

type in_ls = {name:string; file_size:int; right: string list}

type predicted_action =
  | Down
  | Ls of string
  | In_ls of in_ls
  | Up


let next_move (z: cursor zipper) = match z.focus.right with
  | [] -> Up
  | _ -> Down

let parse_in_ls in_ls line =
  if line.[0] = '$' then None
  else
    try Scanf.sscanf line "%d %_s" (fun n ->
        let file_size =  in_ls.file_size + n in
        Some { in_ls with file_size }
      )
    with Scanf.Scan_failure _ ->
      Scanf.sscanf line "dir %s" (fun s ->
          let right = s :: in_ls.right in
          Some { in_ls with right }
        )



let pp_cursor ppf (c:cursor) =
  Format.fprintf ppf "%a|"(Pp.list string) c.right

let pp_remaining ppf zipper =
  pp_cursor ppf zipper.focus;
  List.iter (pp_cursor ppf) zipper.path


let rec parse (th:int) (zipper,state) line =
  match state with
  | Down ->
    let name = Scanf.sscanf line "$ cd %s" (fun s -> s) in
    zipper, Ls name
  | Ls name ->
    assert (line = "$ ls");
    zipper, In_ls {name; file_size=0; right = []}
  | Up ->
    let up = up th zipper in
    if line <> "$ cd .." then Pp.failf "expected \"cd ..\", got :%s@." line;
    up, next_move up
  | In_ls r -> begin
      match parse_in_ls r line with
      | Some in_ls ->
        zipper, In_ls in_ls
      | None ->
        let focus = { left = []; right= r.right; file_size= r.file_size; size_below_threshold = 0 } in
        let zipper = right r.name focus zipper in
        parse th  (zipper, next_move zipper) line
    end

let rec all_up th (z: cursor zipper) = match z.path with
  | [] -> z.focus.size_below_threshold
  | [_] ->
    List.fold_left (fun sum (d:dir) -> sum + d.size_below_threshold) 0 z.focus.left
  | _ -> all_up th (up th z)

let () =
  let th = 100000 in
  let focus: cursor =
     { left = []; right = ["/"]; file_size = 0; size_below_threshold = 0 }
  in
  let zipper = { focus; name=""; dirpath= []; path = [] } in
  let z, _ =  Helper.Input.fold (parse th) (zipper, Down) "7/data/input" in
  let s = all_up th z in
  Format.printf "size=%d@." s
