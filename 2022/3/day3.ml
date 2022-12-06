module Input = Helper.Input
module Pp = Helper.Pp
module Char_set = Set.Make(Char)


let score = function
  | 'a'..'z' as c ->  Char.code c - Char.code 'a' + 1
  | 'A' .. 'Z' as c -> Char.code c - Char.code 'A' + 27
  | c -> Pp.failf "Invalid character:%c" c


let to_set s = String.fold_left (fun set c -> Char_set.add c set) Char_set.empty s

module Part_1() = struct
  let parse s =
    let n = String.length s in
    let half =
      if n mod 2 = 1 then Pp.failf "Odd string length for %S" s
      else n/2
    in
    let s1 = String.sub s 0 half and s2 = String.sub s half half in
    to_set s1, to_set s2

  let line_score (set1, set2) =
    let inter = Char_set.inter set1 set2 in
    match Char_set.elements inter with
    | [x] -> score x
    | [] -> Pp.failf "No common element"
    | a :: b :: _ -> Pp.failf "More than one common element: %c and %c" a b

  let () =
    let add_score score s = score + line_score (parse s) in
    let score = Input.fold add_score 0 "3/data/input" in
    Format.printf "@[score=%d@.@]" score
end

let pp_set ppf set =
  let comma ppf () = Format.fprintf ppf ",@ " in
  let char ppf c = Format.fprintf ppf "%c" c in
  Format.fprintf ppf "{%a}"
  (Format.pp_print_seq ~pp_sep:comma char) (Char_set.to_seq set)

let badge array =
  let (^) = Char_set.inter in
  let inter = array.(0) ^ array.(1) ^ array.(2) in
  match Char_set.elements inter with
  | [x] -> x
  | [] -> Pp.failf "@[No badge: %a, %a, %a@]"
            pp_set array.(0)
            pp_set array.(1)
            pp_set array.(2)
  | a :: b :: _ -> Pp.failf "Multiple badge found: %c %c?" a b

let parse = Array.map to_set

  let () =
    let add_score sc s = sc + score (badge @@ parse s) in
    let score = Input.group_by 3 add_score 0 "3/data/input" in
    Format.printf "@[score=%d@.@]" score
