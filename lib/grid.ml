
module Index = struct
  type t = Vec2.t
  let fold f start {Vec2.x; y} =
    let rec loop f acc i j =
      if i >= x then
        loop f acc 0 (j+1)
      else if j >= y then
        acc
      else
        loop f (f acc (Vec2.make i j)) (i+1) j
    in
    loop f start 0 0


  let (<<) a b = Vec2.( a.x < b.x && a.y < b.y)
  let (<=) a b = Vec2.( a.x <= b.x && a.y <= b.y)

  let iter f {Vec2.x; y} =
    for i = 0 to x -1 do
      for j = 0 to y - 1 do
        f {Vec2.x=i; y=j};
      done;
    done

  let size {Vec2.x; y} = x * y
end


type 'a matrix = { dims: Index.t; data: 'a array array}
let (.!()) mat {Vec2.x;y} =
  mat.data.(x).(y)

let inside v grid = let dims = grid.dims in
  Index.( v << dims && Vec2.zero <= v )

let (.?()) mat pos =
  if inside pos mat then Some mat.!(pos) else None


let make dims x = { dims; data = Array.make_matrix dims.x dims.y x }


let (.!()<-) mat {Vec2.x;y} z =
  mat.data.(x).(y) <- z



let init dims f =
  let mat = make dims (f (Vec2.make 0 0)) in
  Index.iter (fun pos -> mat.!(pos) <- f pos) dims;
  mat



let rec seq mat ~pos ~dir () =
  if inside pos mat then
     Seq.Cons((pos,mat.!(pos)), seq mat ~pos:Vec2.(pos + dir) ~dir)
  else
    Seq.Nil

let fold f acc m =
  Index.fold (fun acc i -> f acc m.!(i)) acc m.dims

let parse_line elt acc x =
  Array.init (String.length x) (fun i -> elt x.[i]) :: acc

let parse elt filename =
  let rev = Input.fold (parse_line elt) [] filename in
  Array.of_list @@ List.rev rev

let gparse elt filename =
  let array = parse elt filename in
  let x = Array.length array in
  let y = Array.length array.(0) in
  let dims = Vec2.make x y in
  { dims; data=array }



let in_grid grid (v:Vec2.t) =
  v.x >= 0 && v.y >= 0 && v.x < Array.length grid && v.y < Array.length grid.(v.x)

module Dir = struct
  type t =
    | Up
    | Down
    | Left
    | Right


  let pp ppf = function
    | Left -> Format.fprintf ppf "←"
    | Right -> Format.fprintf ppf "→"
    | Up -> Format.fprintf ppf "↑"
    | Down -> Format.fprintf ppf "↓"

  let vec2 = function
    | Up -> Vec2.make (-1) 0
    | Down -> Vec2.make (1) 0
    | Left -> Vec2.make 0 (-1)
    | Right -> Vec2.make 0 1
end
