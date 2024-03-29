
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

  let iter f {Vec2.x; y} =
    for i = 0 to x -1 do
      for j = 0 to y - 1 do
        f {Vec2.x=i; y=j};
      done;
    done

  let size {Vec2.x; y} = x * y
end


type 'a matrix = { dims: Index.t; data: 'a array }
let (.!()) mat {Vec2.x;y} =
  mat.data.(x + y * mat.dims.Vec2.x)

let make dims x = { dims; data = Array.make (Index.size dims) x }


let (.!()<-) mat {Vec2.x;y} z =
  mat.data.(x + y * mat.dims.Vec2.x) <- z



let init dims f =
  let mat = make dims (f (Vec2.make 0 0)) in
  Index.iter (fun pos -> mat.!(pos) <- f pos) dims;
  mat


let rec seq mat ~pos ~dir () =
  if Index.(pos << mat.dims && Vec2.make (-1) (-1) << pos) then
     Seq.Cons((pos,mat.!(pos)), seq mat ~pos:Vec2.(pos + dir) ~dir)
  else
    Seq.Nil

let parse_line elt acc x =
  Array.init (String.length x) (fun i -> elt x.[i]) :: acc

let parse elt filename =
  let rev = Input.fold (parse_line elt) [] filename in
  Array.of_list @@ List.rev rev

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
