
type range_map = { source:int; dest:int; width:int }

let parse_range l = Scanf.sscanf l "%d %d %d" (fun dest source width -> { source;dest;width})


let rec validate = function
  | [] | [_] -> ()
  | x :: (y :: _ as rest) ->
    assert (x.source + x.width <= y.source);
    validate rest


let parse_table lines =
  match lines with
  | [] -> assert false
  | _ :: rest ->
    let tbl = List.sort Stdlib.compare @@ List.map parse_range rest in
    validate tbl;
    tbl



let parse_start_part_1 = function
  | [x] -> Scanf.sscanf x "seeds: %r" (Helper.Input.num_list (Fun.const false))
    Fun.id
  | [] -> assert false
  | _ -> assert false


type interval = { start:int; stop:int }

let parse_start_part_2 x =
  let l = parse_start_part_1 x in
  let rec pair = function
    | [] -> []
    | a :: b :: q -> {start=a; stop=a + b} :: pair q
    | [_] -> assert false
  in
  List.sort compare @@ pair l

let parse start data =
  let seeds, tbl =
  Helper.Input.fold_block (fun s -> String.trim s = "")
    (fun (seeds,tables) tbl ->
       if seeds = [] then start tbl, []
       else (seeds, parse_table tbl :: tables)
    ) ([],[]) data
  in
  seeds, List.rev tbl

module Part_1 = struct
  let transfer x tbl =
    match List.find_opt (fun r -> r.source <= x && r.source + r.width > x) tbl with
    | None -> x
    | Some r ->
      let y = x + (r.dest - r.source) in
      Format.eprintf "%d -> %d@." x y;
      y

  let all_transfer tbls x =
    List.fold_left transfer x tbls

  let[@warning "-32"] main data =
    let seeds, tbls = parse parse_start_part_1 data in
    let candidates = List.map (all_transfer tbls) seeds in
    let smallest_location = List.fold_left min Int.max_int candidates in
    Format.printf "smallest location = %d@." smallest_location
end

module Part_2 = struct

  let pp_intv ppf i = Format.fprintf ppf "[%d,%d[" i.start i.stop
  let pp_range ppf r =
    Format.fprintf ppf "[%d,%d[ %+d" r.source (r.source + r.width) (r.dest - r.source)

  let pp_set ppf i =
    Helper.Pp.list pp_intv ppf i

  let rec insert i = function
    | [] -> [i]
    | j :: js ->
      if i.start <= j.start then
        if i.stop > j.start then
          { start = i.start; stop = max i.stop j.stop } :: js
        else
          i :: j :: js
      else if j.stop > i.start then
        { start = i.start; stop = max i.stop j.stop } :: js
      else
        j :: insert i js

  let stop t = t.width + t.source
  let start t = t.source

  let translate x m =
    let delta = m.dest - m.source in
    { start = x.start + delta; stop = x.stop + delta }

  let rec transfer translated input (maps: range_map list) =
    match input, maps with
    | [], _maps -> translated
    | input, [] -> List.fold_left (fun acc i -> insert i acc) translated input
    | i :: is, t :: ts ->
      if i.stop <= start t then
        transfer (insert i translated) is maps
      else if i.start >= stop t then
        transfer translated input ts
      else if i.start >= start t then
        if i.stop <= stop t then
          let i' = translate i t in
          transfer (insert i' translated) is maps
        else
          let i' =  { i with stop = stop t } in
          let i' = translate i' t in
          let r = { i with start = stop t } in
          transfer (insert i' translated) (r::is) ts
      else
        let i' = { i with stop = start t } in
        let r = { i with start = start t } in
        transfer (insert i' translated) (r::is) maps

  let transfer input maps =
    let r = transfer [] input maps in
    Format.eprintf "@[<v 2>Maps:@,%a@;<0 -2>Before:@,@[%a@]@;<0 -2>After:@,%a@]@."
      (Helper.Pp.list pp_range) maps
      pp_set input
      pp_set r;
    r

  let transfer_all ivs tables =
    List.fold_left transfer ivs tables


  let main data =
    let seeds, tbls = parse parse_start_part_2 data in
    let candidates = transfer_all seeds tbls in
    let smallest_location = match candidates with
      | [] -> assert false
      | i :: _ -> i.start
    in
    Format.printf "smallest location = %d@." smallest_location

end

let () = Part_2.main "2023/day5/data/input"
