type order = { repeat:int; from:int; to_:int }

let parse_order s = Scanf.sscanf s "move %d from %d to %d" (fun repeat from to_ ->
    {repeat;from=from - 1;to_ = to_ - 1}
  )

let init n = Array.make n []

let move from to_ state = match state.(from) with
  | [] -> assert false
  | a :: q ->
    let l = state.(to_) in
    state.(from) <- q;
    state.(to_) <- a :: l

let _move_old order state =
  for _i = 1 to order.repeat do move order.from order.to_ state done;
  state

let move_new order state =
  let rec move remain stack  =
    if remain = 0 then
      state.(order.from) <- stack
    else match stack with
      | [] -> assert false
      | next :: stack ->
        move (remain - 1) stack;
        state.(order.to_) <- next :: state.(order.to_)
  in
  move order.repeat state.(order.from);
  state

let top state =
  Array.map List.hd state

let none _ppf () = ()
let comma ppf () = Format.fprintf ppf ",@ "
let _int ppf = Format.fprintf ppf "%d"
let char ppf = Format.fprintf ppf "%c"

let _list ?(sep=comma) elt ppf x = Format.pp_print_list ~pp_sep:sep elt ppf x
let array ?(sep=comma) elt ppf x = Format.pp_print_seq ~pp_sep:sep elt ppf (Array.to_seq x)


let check_horizontal s = String.contains s '['

let parse_horizontal s =
  let len = (String.length s + 1) / 4 in
  Array.init len (fun i ->
      let pos = 1 + 4 * i in
      if i >= String.length s then [] else
        let c = s.[pos] in
        if c = ' ' then [] else [c]
    )

type state =
  | First of char list array list
  | Number of char list array
  | Order of char list array

let fill_array lists =
  let len = List.fold_left (fun n l -> max n @@ Array.length l) 0 lists in
  let array = init len  in
  let fill_line line =
    Array.iteri (fun n l ->  array.(n) <- l @ array.(n) ) line
  in
  List.iter fill_line lists;
  array

let folder move state s =
  match state with
  | First st ->
    if check_horizontal s then
      First (parse_horizontal s :: st)
    else
      let array = fill_array st in
      Number array
  | Number st -> Order st
  | Order st ->
    Order(move (parse_order s) st)

let () =
  let result = Helper.Input.fold (folder move_new) (First []) "./5/data/input" in
  match result with
  | Order a ->
    Format.printf "@[result=[%a]@]@." (array ~sep:none char) (top a)
  | _ -> assert false
