

type t =
  | Ash
  | Rock

let parse_char = function
  | '#' -> Rock
  | '.' -> Ash
  | _ -> assert false

let pp_char ppf = function
  | Rock -> Format.fprintf ppf "#"
  | Ash -> Format.fprintf ppf "."

let pp_line ppf a =
  Array.iter (pp_char ppf) a

let cut ppf () = Format.fprintf ppf "@,"

let pp_mat ppf m =
  Format.fprintf ppf "@[<v>%a@]"
    (Format.pp_print_seq ~pp_sep:cut pp_line) (Array.to_seq m)


let parse_line l = Array.init (String.length l) (fun i -> parse_char l.[i])

let parse_block acc ls =
  let l = List.fold_left (fun acc x -> parse_line x :: acc)
    [] ls
  in
  let m = Array.of_list @@ List.rev l in
  m :: acc

let rec dist max_err err pos x y =
  if err > max_err then None
  else if pos >= Array.length x then Some (max_err - err)
  else if x.(pos) = y.(pos) then dist max_err err (pos+1) x y
  else dist max_err (1+err) (1+pos) x y

let dist mx x y =
  if Array.length x = Array.length y then
    dist mx 0 0 x y
  else
    None

let rec sym fuel m i j =
  if i < 0 || j >= Array.length m then fuel = 0
  else
    match dist fuel m.(i) m.(j) with
    | None -> false
    | Some fuel ->
      sym fuel m (i-1) (j+1)



let sym_start max_dist m i =
  i > 0 && sym max_dist m (i-1) i

let sym_index max_dist m =
  Array.find_mapi (fun i _ -> if sym_start max_dist m i then Some i else None) m

let transpose m =
  Array.init (Array.length m.(0)) (fun j ->
      Array.init (Array.length m) (fun i ->
          m.(i).(j)
        )
    )

let reflexion max_dist m =
  Format.eprintf "@.%a@." pp_mat m;
  match sym_index max_dist m with
  | Some x -> 100 * x
  | None ->
    let m = transpose m in
    match sym_index max_dist m with
    | Some x -> x
    | None -> assert false

let parse data =
  Helper.Input.fold_block
    (fun x -> String.trim x = "" )
    parse_block
    []
    data

let main max_dist data =
  let ms = parse data in
  let score = List.fold_left (fun sc m -> sc + reflexion max_dist m) 0 ms in
  Format.printf "-----@.score=%d@." score

let () = main 1 "2023/day13/data/input"
