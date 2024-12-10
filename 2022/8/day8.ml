open Helper.Grid
module G = Helper.Grid
module Vec = Helper.Vec2

type state = Visible | Hidden
let update ~dir ~start visibility sizes =
  let f mx (pos,size) =
    if size <= mx then
      mx
    else begin
        visibility.!(pos) <- Visible;
        size
      end
  in
  let _ = Seq.fold_left f (-1) (seq sizes ~pos:start ~dir) in
  ()

let compute_visibility sizes =
  let {Vec.x=dim_x; y=dim_y} = sizes.dims in
  let visibility = make sizes.dims Hidden in
  for i = 0 to (dim_x - 1) do
    update ~dir:(Vec.make 0 1) ~start:(Vec.make i 0) visibility sizes;
    update ~dir:(Vec.make 0 (-1)) ~start:(Vec.make i (dim_y - 1)) visibility sizes;
  done;
  for j = 0 to (dim_y - 1) do
    update ~dir:(Vec.make 1 0) ~start:(Vec.make 0 j) visibility sizes;
    update ~dir:(Vec.make (-1) 0) ~start:(Vec.make (dim_x - 1) j) visibility sizes;
  done;
  visibility

let scenic_score sizes start =
  let size = sizes.!(start) in
  let f (size,n) (_pos,new_size) =
    match size with
    | None -> (size, n)
    | Some size ->
      if new_size >= size then (None,n+1)
      else (Some size, n + 1)
  in
  let dir_score dir =
    let _, n = Seq.fold_left f (Some size,0) (seq sizes ~pos:Vec.(start+dir) ~dir) in
    n
  in
  dir_score (Vec.make 0 1)
  * dir_score (Vec.make 1 0)
  * dir_score (Vec.make 0 (-1))
  * dir_score (Vec.make (-1) 0)

let n_visible visibility =
  G.fold
    (fun sum v -> sum + (if v = Visible then 1 else 0))
    0 visibility

let parse_line lines line =
  let array =
    Array.init (String.length line)
      (fun i -> int_of_string (String.sub line i 1))
  in
  array :: lines

let score_scene sizes before pos =
  let score = scenic_score sizes pos in
  max score before


let () =
  let lines = Helper.Input.fold parse_line [] "8/data/input" in
  let arrays = Array.of_list (List.rev lines) in
  let dim_x = Array.length arrays in
  let dim_y = Array.length (arrays.(0)) in
  let () = assert (Array.for_all (fun a -> Array.length a = dim_y ) arrays) in
  let mat = init (Vec.make dim_x dim_y) (fun v -> arrays.(v.x).(v.y)) in
  let visibility = compute_visibility mat in
  let scenic_score = Index.fold (score_scene mat) (-1) mat.dims in
  Format.printf "n visible=%d score=%d@." (n_visible visibility) scenic_score
