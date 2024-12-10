open Helper



let parse filename = Grid.gparse (fun c -> Char.code c - Char.code '0') filename

module Pos_set = Set.Make(Vec2)

type 'a  memo =
  | Not_visited
  | Done of 'a

let up = Vec2.make 0 1
let right = Vec2.make 1 0
let down, left = Vec2.(~- up, ~-right)


let neighborhood = [ up; down; left; right ]

type 'a monoid = {
  zero:'a;
  inj: Vec2.t -> 'a;
  add: 'a -> 'a -> 'a;
  score: 'a -> int
}

let monoid_one =
  { zero = Pos_set.empty;
    inj = Pos_set.singleton;
    add = Pos_set.union;
    score = Pos_set.cardinal
  }

let monoid_two = {
  zero = 0;
  inj = (Fun.const 1);
  add = (+);
  score =  Fun.id;
}

let rec fill_path monoid hg p pos =
  match p.Grid.!(pos) with
  | Done s -> s
  | Not_visited ->
    let set =
      let h = hg.Grid.!(pos) in
      if h = 9 then monoid.inj pos
      else
          List.fold_left (fun set n ->
              let pos = Vec2.( n + pos ) in
             monoid.add set (candidate monoid hg p h pos)
            ) monoid.zero neighborhood
    in
    p.Grid.!(pos) <- Done set; set

and candidate monoid hg p h c =
  if not (Grid.inside c hg) then monoid.zero
  else if hg.Grid.!(c) - h <> 1 then  monoid.zero
  else fill_path monoid hg p c


let fill_all monoid hg =
  let p = Grid.make hg.Grid.dims Not_visited in
  Grid.Index.iter (fun pos -> ignore @@ fill_path monoid hg p pos) p.dims;
  p

let score_pos m height paths pos =
  let open Grid in
  if height.!(pos) <> 0 then 0 else
    match paths.!(pos) with
    | Not_visited -> assert false
    | Done set -> m.score set

let score m h p =
  Grid.Index.fold (fun s i -> s + score_pos m h p i) 0 h.Grid.dims

let fill_and_score m h =
  let p = fill_all m h in
  score m h p


let () =
  let h = parse Sys.argv.(1) in
  Format.printf "@[score 1: %d, score 2:%d @]@."
    (fill_and_score monoid_one h)
    (fill_and_score monoid_two h)
