type t = { x:int; y:int }

let dist_inf v w = max (abs (v.x - w.x)) (abs (v.y - w.y))
let dist_1 v w = (abs (v.x - w.x)) + (abs (v.y - w.y))


let compare: t -> t -> int = Stdlib.compare
let map f v = { x = f v.x; y = f v.y }
let make x y = {x;y}
let (+) v w = { x= v.x + w.x; y = v.y + w.y }
let (-) v w = { x= v.x - w.x; y = v.y - w.y }
let pp ppf v = Format.fprintf ppf "(%d %d)" v.x v.y
