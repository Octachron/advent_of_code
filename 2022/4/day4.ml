module Input = Helper.Input
type interval = { min:int; max:int }

let make x y = { min = min x y; max = max x y }
let parse line = Scanf.sscanf line "%d-%d,%d-%d" (fun a b x y -> make a b, make x y)
let (<<) x y =
  x.min <= y.min && x.max >= y.max

let redundant (x,y) = x << y || y << x

let left_overlap (x,y) =
  x.min <= y.min && x.max >= y.min

let swap (x,y) = (y,x)

let overlap pair =
   left_overlap pair || left_overlap (swap pair)

let count classifier =
  let count n line =
    n + if classifier (parse line) then 1 else 0
  in
  let redundants = Input.fold count 0 "4/data/input" in
  Format.printf "redudant assignments:%d@." redundants

let _part1 () = count redundant
let part2 () = count overlap
let () = part2 ()
