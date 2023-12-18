let hash acc char =
  let acc = acc + Char.code char in
  let acc = 17 * acc in
  acc mod 256


let hash' s  = String.fold_left hash 0 s

module[@warning "-32"] Part_1 = struct

  let fold_part acc line =
    acc + String.fold_left hash 0 line

  let fold acc line = List.fold_left fold_part acc (String.split_on_char ',' line)

  let main data =
    let score = Helper.Input.fold fold 0 data in
    Format.printf "score=%d@."
      score
end


type lens = { label:string; pos:int; focal:int }

type update =
  | Remove of string
  | Add of lens

let parse s =
  if s.[String.length s - 1 ] = '-' then
    Scanf.sscanf s "%s@-" (fun s -> Remove s)
  else
    Scanf.sscanf s "%s@=%d" (fun lbl focal -> Add { pos = hash' lbl; label=lbl; focal })


let[@tail_mod_cons] rec remove lbl = function
  | [] -> []
  | a :: q -> if a.label = lbl then q else a :: remove lbl q

let[@tail_mod_cons] rec add lens = function
  | [] -> [lens]
  | a :: q -> if a.label = lens.label then lens :: q else a :: add lens q


let update boxes s =
  match parse s with
  | Remove label ->
    let n = hash' label in
    let b = boxes.(n) in
    boxes.(n) <- remove label b
  | Add lens ->
    let b = boxes.(lens.pos) in
    boxes.(lens.pos) <- add lens b

let update_line boxes () l =
  List.iter (update boxes) (String.split_on_char ',' l)

let box_score box_n b =
  let _, score = List.fold_left (fun (pos, acc) lens ->
      let score = box_n * pos * lens.focal in
      Format.eprintf "Box %d; slot %d, %s (%d)= %d@." box_n pos lens.label lens.focal score;
      1 + pos,
      acc + score
    ) (1, 0) b
  in
  score

let score boxes =
  let _, score =
  Array.fold_left (fun (i,acc) b ->
    1 + i,
    acc + box_score i b
      ) (1,0) boxes
  in
  score


let main data =
  let boxes = Array.make 256 [] in
  let () = Helper.Input.fold (update_line boxes) () data in
  Format.eprintf "score=%d@." (score boxes)

let () = main "2023/day15/data/input"
