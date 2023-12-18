
type dir =
  | Up
  | Down
  | Left
  | Right



let index = function
  | Up -> 0
  | Down -> 1
  | Left -> 2
  | Right -> 3

module V = Helper.Vec2

let dir = function
  | Up -> V.make (-1) 0
  | Down -> V.make (1) 0
  | Left -> V.make 0 (-1)
  | Right -> V.make 0 1


type optics =
  | V_splitter
  | H_splitter
  | Empty
  | Diag
  | Antidiag

(* /  *)
let reflect_diag = function
  | Right -> Up
  | Up -> Right
  | Down -> Left
  | Left -> Down

(* \ *)
let reflect_antidiag = function
  | Right -> Down
  | Up -> Left
  | Down -> Right
  | Left -> Up


type cell = { optics:optics; lights: bool array  }

let (.%()) grid (v:V.t) = grid.(v.x).(v.y)

let in_grid grid (v:V.t) =
  v.x >= 0 && v.y >= 0 && v.x < Array.length grid && v.y < Array.length grid.(v.x)


let move pos d = V.(pos + dir d, d)

let encounter grid queue (pos,dir) =
  if not (in_grid grid  pos) then queue
  else
    let cell = grid.%(pos) in
    if cell.lights.(index dir) then queue
    else begin
      cell.lights.(index dir) <- true;
      match cell.optics, dir with
      | Diag, _ -> move pos (reflect_diag dir) :: queue
      | Antidiag, _ ->
        let dir = reflect_antidiag dir in
        move pos dir :: queue
      | Empty, _ | V_splitter, (Up|Down) | H_splitter, (Right|Left) ->
        move pos dir :: queue
      | V_splitter, (Right|Left) ->
        move pos Up :: move pos Down :: queue
      | H_splitter, (Up|Down) ->
        move pos Left :: move pos Right :: queue
    end

let rec compute grid = function
  | [] -> ()
  | a :: q ->
    compute grid (encounter grid q a)


let zero () = Array.make 4 false
let mk o = { optics=o; lights = zero () }

let parse_optics = function
  | '.' -> mk Empty
  | '/' -> mk Diag
  | '\\' -> mk Antidiag
  | '-' -> mk H_splitter
  | '|' -> mk V_splitter
  | _ -> assert false

let parse_line s = Array.init (String.length s) (fun i -> parse_optics s.[i])

let parse data =
  let l = Helper.Input.fold (fun acc s -> parse_line s :: acc ) [] data in
  l |> List.rev |> Array.of_list

let score_line acc l =
  Array.fold_left (fun s c ->
      if Array.exists Fun.id c.lights then 1 + s else s
    ) acc l

let score g =
  Array.fold_left score_line 0 g

let[@warning "-32"] part_1_main data =
  let grid = parse data in
  let () = compute grid [V.make 0 0, Right] in
  Format.printf "score=%d@." (score grid)

let refresh grid =
  Array.iter (Array.iter (fun c -> Array.iteri (fun i _ -> c.lights.(i) <- false ) c.lights )) grid

let test sc grid start =
  refresh grid; compute grid start;
  sc := max !sc (score grid)

let range a f = for i = 0 to Array.length a - 1 do f i done

let top sc g =
  range g.(0) (fun i -> test sc g [ V.make 0 i, Down ])

let bottom sc g =
  range g.(0) (fun i -> test sc g [ V.make (Array.length g - 1) i, Up ])

let left sc g =
  range g (fun i -> test sc g [ V.make i 0, Right ])

let right sc g =
  range g (fun i -> test sc g [ V.make i (Array.length g.(0) - 1), Left ])

let main data =
  let g = parse data in
  let sc = ref 0 in
  top sc g; bottom sc g; left sc g; right sc g;
  Format.printf "score=%d@." !sc


let () = main "2023/day16/data/input"
