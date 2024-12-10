

module Graph = Map.Make(Int)
module Edges = Set.Make(Int)


let (.%()) g x = match Graph.find_opt x g with
  | None -> Edges.empty
  | Some x -> x

type input = { edges: Edges.t Graph.t; update : int list list }

let parse_line acc s =
  match Scanf.sscanf_opt s "%d|%d" (fun x y -> x, y) with
  | Some (before, after) ->
    let edges = Graph.add before (Edges.add after acc.edges.%(before)) acc.edges in
    { acc with edges }
  | None ->
    let nums = String.split_on_char ',' s in
    if String.trim s = "" then acc else
    let update = List.map int_of_string nums :: acc.update in
    { acc with update }

module Wrong = struct
let rec in_order visited path g x y =
  let elements = Edges.elements g.%(x) in
  let visited = Edges.add x visited in
  in_order_elements path visited g x elements y
and in_order_elements path visited g x xs y =
  match xs with
  | [] -> None
  | a :: q ->
    if Edges.mem a visited then in_order_elements path visited g x q y
    else if a = y then Some (List.rev (a::path))
    else
      let visited = Edges.add a visited in
      match  in_order visited (a::path) g a y with
      | None ->  in_order_elements path visited g x q y
      | Some _ as x -> x

let in_order g x y = in_order Edges.empty [x] g x y

let[@warning "-32"] rec validate g l = match l with
  | [] | [_] -> true
  | a :: (b :: _ as q) ->
    match in_order g b a with
    | Some _ -> false
    | None ->  validate g q
end

let rec validate_2 g set = function
  | [] -> true
  | x :: q ->
      Edges.is_empty (Edges.inter set g.%(x)) &&
      validate_2 g (Edges.add x set) q

let simplify g points =
  Graph.map (Edges.inter points) g

let rec toposort_d g remaining order current =
  if not (Edges.mem current remaining) then
    remaining, order
  else
    let r, o = Edges.fold (fun x (r,o) -> toposort_d g r o x)
      g.%(current)
      (remaining,order)
    in
    let remaining = Edges.remove current r in
    let order = current :: o in
    remaining, order

let rec toposort_more g remaining order =
    if Edges.is_empty remaining then order
    else
      let current = Edges.choose remaining in
      let r, o = toposort_d g remaining order current in
      toposort_more g r o

let toposort g points = toposort_more g points []


let start = { update = []; edges = Graph.empty }

let score_line l =
      let a = Array.of_list l in
      a.((Array.length a)/2)

let score g acc line =
    if validate_2 g Edges.empty line then
      acc + score_line line
    else
      acc

let score2 g acc line =
    if validate_2 g Edges.empty line then acc
    else
      let () = Format.eprintf "@[line %a@]@." Fmt.(Dump.list int) line in
      let points = Edges.of_list line in
      let g = simplify g points in
      let l = toposort g points in
      let () = Format.eprintf "@[Reordered line:@ %a (%B)@]@."
          Fmt.(Dump.list int) l
          (validate_2 g Edges.empty l)
      in
      acc + score_line l

let () =
  let input = Helper.Input.fold parse_line start Sys.argv.(1) in
  let iu = List.rev input.update in
  let score = List.fold_left (score input.edges) 0 iu in
  let score2 = List.fold_left (score2 input.edges) 0 iu in
  Format.printf "score = %d, score part 2 = %d@." score score2
