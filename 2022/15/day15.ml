module Vec = Helper.Vec2

type vec = Vec.t = {x:int; y:int}

type scanner = { position: Vec.t; closest: Vec.t; dist:int }

[@@@warning "-32"]

let create position closest =
  { position; closest; dist = Vec.dist_1 position closest }

module Scanner_set = Set.Make(struct type t = scanner let compare = Stdlib.compare end)

module Beacon_set = Set.Make(Int)

type interval = { left: int; right:int }
let pp_interval ppf i =
  Format.fprintf ppf "[%d,%d]" i.left i.right


module Disjoint_Interval_set = Set.Make(struct
    type t = interval
    let compare s1 s2 = Stdlib.compare s1.left s2.left
end)

module Part_one() = struct
  let inter i1 i2 =
    i1.right >= i2.left && i1.left <= i2.right
    || i2.right > i1.left && i2.left <= i1.right

  let merge_i x y = { left = min x.left y.left; right = max x.right y.right }

  let rec merge dis i = match dis with
    | [] -> [i]
    | a :: q ->
      if inter i a then
        merge q (merge_i i a)
      else if i.left > a.left then
        a :: i :: q
      else
        a :: merge q i

  let add_interval_at_x x (beacons,dis) scanner =
    let size = scanner.dist - abs (scanner.position.x - x) in
    let beacons = if scanner.closest.x = x then Beacon_set.add scanner.closest.y beacons else beacons in
    beacons,
    if size >= 0 then
      merge dis {left = scanner.position.y - size; right = scanner.position.y + size }
    else
      dis

  let len dis = List.fold_left (fun s i -> s + i.right - i.left + 1) 0 dis
end


type rect =
  { x_min:int; x_max:int; y_min:int; y_max:int }


let covered {x_min;x_max;y_min;y_max} scanner =
  let cover_point x y =
    Vec.dist_1 (Vec.make x y) scanner.position <= scanner.dist
  in
  cover_point x_min y_min
  && cover_point x_max y_max
  && cover_point x_min y_max
  && cover_point x_max y_min


let is_point rect = rect.x_min = rect.x_max && rect.y_min = rect.y_max

let split rect =
  let mid_x = (rect.x_min + rect.x_max) / 2 in
  let mid_y = (rect.y_min + rect.y_max) / 2 in
  match mid_x = rect.x_min, mid_y = rect.y_min with
  | true, true ->
    [{ rect with x_max = mid_x; y_max = mid_y }]
  | true, false ->
    [{rect with y_min = mid_y}; {rect with y_max = mid_y} ]
  | false, true ->
    [{rect with x_min = mid_x}; {rect with x_max = mid_x} ]
  | false, false ->
    [
      { rect with x_max = mid_x; y_max = mid_y };
      { rect with y_min = mid_y; x_max = mid_x };
      { rect with x_min = mid_x; y_max = mid_y };
      { rect with y_min = mid_y; x_min = mid_x };
    ]


let pp_rect ppf r =
  if is_point r then
    Format.fprintf ppf "{%d %d}" r.x_min r.y_min
  else
    Format.fprintf ppf "[%d, %d] × [%d, %d]" r.x_min r.x_max r.y_min r.y_max

let rec search scanners rect =
  match List.exists (covered rect) scanners with
  | true -> None
  | false ->
    if is_point rect then
      Some rect
    else
      List.find_map (search scanners) (split rect)



let parse_line scanners s =
  Scanf.sscanf s "Sensor at x=%d, y=%d: closest beacon is at x=%d, y=%d"
    (fun sx sy bx by ->
       let scanner = create (Vec.make sx sy) (Vec.make bx by) in
       scanner :: scanners
    )


let () =
  let width = 4_000_000 in
  let scanners = Helper.Input.fold parse_line [] "15/data/input" in
  let max_coord = width in
  let rect = search scanners { x_min=0; y_min=0; x_max=max_coord; y_max=max_coord } in
  match rect with
  | None -> assert false
  | Some rect ->
    Format.printf "point: %d %d = %d@." rect.x_min rect.y_min (rect.x_min * width + rect.y_min)
