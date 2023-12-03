type case =
  | Dot
  | Symbol
  | Star
  | Digit of int

let parse_case = function
  | '0' .. '9' as c -> Digit (Char.code c - Char.code '0')
  | '.' -> Dot
  | '*' -> Star
  | _ -> Symbol

let parse_line line =
  Array.init (String.length line) (fun n -> parse_case line.[n])

open Helper

let read f =
  Array.of_list @@ List.rev @@ Input.fold (fun acc l ->
    parse_line l :: acc
  ) [] f



let (.?()<-) a (i,j) x =
  if i < 0 || i >= Array.length a || j < 0 || j >= Array.length a.(i) then ()
  else a.(i).(j) <-  x

module Part_one = struct
  let mask a m (i,j) =
    match a.(i).(j) with
    | Dot | Digit _ -> ()
    | Star | Symbol ->
      let ijs =
        let (let*) l f = List.concat_map f l in
        let delta = [-1;0;1] in
        let* i_delta = delta in
        let* j_delta = delta in
        [ i +i_delta, j + j_delta ]
      in
      List.iter (fun ij -> m.?(ij) <- true ) ijs

  let mask_array a =
    let lx = Array.length a in
    let ly = Array.length a.(0) in
    let m = Array.make_matrix lx ly false in
    for i = 0 to lx - 1 do
      for j = 0 to ly - 1 do
        mask a m (i,j)
      done
    done;
    m


  let commit score (total,valid) =
    if valid then total + score else score

  let rec score_line score (total,valid as num) pos len ml l =
    if pos >= len then commit score num
    else
      match l.(pos) with
      | Dot | Symbol | Star -> score_line (commit score num) (0,false) (1+pos) len ml l
      | Digit d ->
        let valid = valid || ml.(pos) in
        let total = 10 * total + d in
        score_line score (total, valid) (1+pos) len ml l

  let rec score m a i sc =
    if i >= Array.length m then sc
    else
      let sc = score_line sc (0,false) 0 (Array.length m.(i)) m.(i) a.(i) in
      score m a (i+1) sc

  let[@warning "-32"] main () =
    let cases = read "2023/day3/data/input" in
    let mask = mask_array cases in
    let s = score mask cases 0 0 in
    Format.printf "score=%d@." s
end

module Part_2 = struct

  let (.?()) a i =
    if i >= Array.length a || i < 0 then false
    else match a.(i) with
      | Digit _ -> true
      | Symbol | Star | Dot -> false

  let rec left_digit l i =
    if i = 0 then i
    else
      match l.(i-1) with
      | Digit _ -> left_digit l (i-1)
      | _ -> i

  let rec number_starting_at before l i =
    if i >= Array.length l then before
    else match l.(i) with
      | Digit d -> number_starting_at (10*before + d) l (i+1)
      | _ -> before

  let number_around l i =
    number_starting_at 0 l (left_digit l i)

  let numbers_around l i =
    match l.?(i-1), l.?(i), l.?(i+1) with
    | true, false, true ->
      [number_around l (i-1); number_around l (i+1)]
    | true, true, _ -> [number_around l i]
    | true, false , false -> [number_around l (i-1)]
    | false, true, _ -> [number_around l i]
    | false, false, true -> [number_around l (i+1)]
    | false, false, false -> []

  let gear_numbers m i j =
    let maybe i =
      if i >= 0 && i < Array.length m then numbers_around m.(i) j
      else []
    in
    List.concat_map maybe [i-1;i;i+1]

  let gear_score s m i j =
    match m.(i).(j) with
    | Symbol | Dot | Digit _ -> ()
    | Star ->
      match gear_numbers m i j with
      | [x;y] -> s:= !s + x * y
      | _ -> ()

  let total_gear_score m =
    let s = ref 0 in
    for i = 0 to Array.length m -1 do
      for j = 0 to Array.length m.(i) - 1 do
        gear_score s m i j
      done;
    done;
    !s

  let main ()  =
    let cases = read "2023/day3/data/input" in
    let score = total_gear_score cases in
    Format.printf "gear score=%d@." score

  
end

let () = Part_2.main ()
