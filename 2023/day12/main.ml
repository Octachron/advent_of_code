
type cell =
  | Damaged
  | Ok
  | Unknown

let parse_char = function
  | '#' -> Damaged
  | '.' -> Ok
  | '?' -> Unknown
  | _ -> assert false

let parse_tbl x = Array.init (String.length x) (fun i -> parse_char x.[i])

let parse_line l =
  Scanf.sscanf l "%s %r" Helper.Input.comma_int_list (fun x s ->
      parse_tbl x, s)


module Memo = Map.Make(struct
    type t = int * int list
    let compare = Stdlib.compare
  end)

let register l pos (memo,count) =
  Memo.add (pos,l) count memo, count

let rec backtrack_count memo a pos l =
  let register = register l pos in
  let memo_back a pos l = register (backtrack_count memo a pos l) in
  let memo_take x a pos l = register (take memo x a pos l) in
  match Memo.find_opt (pos,l) memo with
  | Some x -> memo, x
  | None ->
  if pos >= Array.length a then
    if List.is_empty l then memo, 1 else memo, 0
  else match a.(pos), l with
  | Damaged, [] -> memo, 0
  | Ok, _ -> memo_back a (pos+1) l
  | Unknown, [] -> memo_back  a (pos+1) l
  | Damaged, x :: l -> memo_take (x-1) a (pos + 1) l
  | Unknown, x :: q ->
    let memo, count2 = take memo (x-1) a (pos + 1) q in
    let memo, count1 = backtrack_count memo a (pos+1) l in
    register (memo,count1 + count2)
and take memo n a pos l =
  if pos >= Array.length a then
    if List.is_empty l && n = 0 then memo, 1 else memo,0
  else if n = 0 then
    space_required memo a pos l
  else
    match a.(pos) with
    | Ok -> memo, 0
    | Damaged | Unknown -> take memo (n-1) a (pos+1) l
and space_required memo a pos l =
  if pos >= Array.length a then
    if List.is_empty l then memo, 1 else memo, 0
  else match a.(pos) with
    | Ok | Unknown -> backtrack_count memo a (1+pos) l
    | Damaged -> register l pos (memo,0)

let rec repeat_seq sep n seq = match n with
  | 0 -> Seq.empty
  | 1 -> seq
  | n -> Seq.append seq (Seq.append sep @@ repeat_seq sep (n-1) seq)

let fold n acc x =
  let tbl, req = parse_line x in
  let tbl = Array.of_seq @@ repeat_seq (Seq.return Unknown) n @@ Array.to_seq tbl in
  let req = List.of_seq @@ repeat_seq Seq.empty n @@ List.to_seq req in
  let _, count = backtrack_count Memo.empty tbl 0 req in
  Format.eprintf "%s: %d possibilities@." x count;
  acc + count


let main data =
  let count = Helper.Input.fold (fold 5) 0 data in
  Format.printf "%d@." count

let () = main "2023/day12/data/input"
