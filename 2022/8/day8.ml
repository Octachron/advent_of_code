
module Index = struct
  type t = int * int

let fold f start (dim_x, dim_y) =
  let rec loop f acc i j =
    if i >= dim_x then
      loop f acc 0 (j+1)
    else if j >= dim_y then
      acc
    else
      loop f (f acc (i,j)) (i+1) j
  in
  loop f start 0 0


  let (+) (a,b) (x,y) = a + x, b + y
  let (<<) (a,b) (x,y) = a < x && b < y

let iter f (dim_x,dim_y) =
  for i = 0 to dim_x -1 do
    for j = 0 to dim_y - 1 do
       f (i,j);
    done;
  done


end


type 'a matrix = { dims: Index.t; data: 'a array }
let (.!()) mat (x,y) =
  mat.data.(x + y * fst mat.dims)

let make dims x = { dims; data = Array.make (fst dims * snd dims) x }


let (.!()<-) mat (x,y) z =
  mat.data.(x + y * fst mat.dims) <- z



let init dims f =
  let mat = make dims (f (0,0)) in
  Index.iter (fun pos -> mat.!(pos) <- f pos) dims;
  mat


let rec seq mat ~pos ~dir () =
  if Index.(pos << mat.dims && (-1,-1) << pos) then
     Seq.Cons((pos,mat.!(pos)), seq mat ~pos:Index.(pos + dir) ~dir)
  else
    Seq.Nil

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
  let dim_x, dim_y = sizes.dims in
  let visibility = make sizes.dims Hidden in
  for i = 0 to (dim_x - 1) do
    update ~dir:(0,1) ~start:(i,0) visibility sizes;
    update ~dir:(0,-1) ~start:(i,dim_y - 1) visibility sizes;
  done;
  for j = 0 to (dim_y - 1) do
    update ~dir:(1,0) ~start:(0,j) visibility sizes;
    update ~dir:(-1,0) ~start:(dim_x - 1,j) visibility sizes;
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
    let _, n = Seq.fold_left f (Some size,0) (seq sizes ~pos:Index.(start+dir) ~dir) in
    n
  in
  dir_score (0,1) * dir_score (1,0) * dir_score (0,-1) * dir_score (-1,0)

let n_visible visibility =
  Array.fold_left
    (fun sum v -> sum + (if v = Visible then 1 else 0))
    0 visibility.data

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
  let mat = init (dim_x,dim_y) (fun (i,j) -> arrays.(i).(j)) in
  let visibility = compute_visibility mat in
  let scenic_score = Index.fold (score_scene mat) (-1) mat.dims in
  Format.printf "n visible=%d score=%d@." (n_visible visibility) scenic_score
