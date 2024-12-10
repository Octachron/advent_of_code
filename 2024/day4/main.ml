open Helper
let parse f = Grid.gparse Fun.id f

let rec word_in_direction w wpos g dir gpos =
  (wpos >= String.length w) ||
  (Grid.inside gpos g
   && g.Grid.!(gpos) = w.[wpos]
   && word_in_direction w (wpos+1) g dir Vec2.(gpos + dir)
  )

let word_in_dir w g gpos dir = word_in_direction w 0 g dir gpos

let up = Vec2.make 0 1
let right = Vec2.make 1 0
let down, left = Vec2.(~- up, ~-right)


let all_dir = let open Vec2 in
  [| up; down; left; right; up + left; up + right; down + left; down + right |]

let _xcount0 pos mat =
  let open Vec2 in let open Grid in
  if not (inside Vec2.(pos + up + right) mat && inside (pos - up -right) mat && mat.!(pos)='A') then 0
  else
    let diags = [pos + up + left; pos + up + right; pos + down + left; pos + down + right] in
    let count l = List.fold_left (fun acc x -> if mat.!(x) = l then succ acc else acc) 0 diags in
    if count 'M' = 2 && count 'S' = 2 then 1 else 0

let xcount1 pos mat =
  let open Vec2 in let open Grid in
  if not (inside Vec2.(pos + up + right) mat && inside (pos - up -right) mat && mat.!(pos)='A') then 0
  else
    let diags = [pos + up + left; pos + up + right; pos + down + left; pos + down + right] in
    match List.map (fun pos -> mat.!(pos)) diags with
    | [ 'M'; 'M'; 'S'; 'S' ] | [ 'S'; 'S'; 'M'; 'M' ]
    | [ 'M'; 'S'; 'M'; 'S' ] | [ 'S'; 'M'; 'S'; 'M' ]
      -> 1
    | _ -> 0


let[@warning "-32"] word_count w g gpos =
  Array.fold_left (fun acc dir -> if word_in_dir w g gpos dir then acc + 1 else acc) 0 all_dir

let word_count_in_grid _w g = Grid.Index.fold
    (fun acc i -> acc + xcount1 i g) 0 g.dims

let () =
  let filename = Sys.argv.(1) in
  let g = parse filename in
  let wc = word_count_in_grid "XMAS" g in
  Format.printf "%d xmas@." wc
