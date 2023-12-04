

module Int_map = Map.Make(Int)
module Int_set = Set.Make(Int)

type card = { id: int; w: int list; n: int list }

let rec num_list bf =
  if Scanf.Scanning.end_of_input bf || Scanf.bscanf bf "%0c" Fun.id = '|' then [] else
  Scanf.bscanf bf "%d %r" num_list (fun s l -> s :: l )

let parse_line line =
  Scanf.sscanf line "Card %d: %r | %r" num_list num_list (fun id w n -> { id; w; n})


let n_win t =
    let ws = Int_set.of_list t.w in
    t.n |> List.to_seq |> Seq.filter (fun x -> Int_set.mem x ws) |> Seq.length


module Part_one  = struct
  let score c =
    let n = n_win c in
    assert (n< 60);
    1 lsl (n-1)

  let[@warning "-32"] main data =
    let score = Helper.Input.fold (fun s line ->
        let new_score = score (parse_line line) in
        let s' = s + new_score in
        if s' < s then (Format.eprintf "%d + %d = %d?@." s new_score s'; assert false);
        s'
      )
        0
        data
    in
    Format.printf "score=%d@." score
end

module Part_2 = struct
  let (.!()) m card = Option.value ~default:0 (Int_map.find_opt card.id m)
  let add ~card n m = Int_map.add card.id (n + m.!(card)) m
  let copy_at_n m card =
    let m = add ~card 1 m in
    let wins = n_win card in
    let win_range = Seq.ints (1 + card.id) |> Seq.take wins in
    let self_copies = m.!(card) in
    Format.printf "Card %d, %d copies, win %n copies@." card.id self_copies wins;
    Seq.fold_left (fun m id ->
        let card = { id; w=[]; n=[] } in
        add ~card self_copies m
      ) m win_range

  let score copies = Int_map.fold (fun k v acc ->
      Format.eprintf "card %d = %d copies@." k v;
      v + acc) copies 0

  let main data =
    let copies = Helper.Input.fold (fun m line ->
        let card = parse_line line in
        copy_at_n m card
      ) Int_map.empty data
    in
    Format.printf "total number_of_card=%d@." (score copies)
  
end

let () = Part_2.main "2023/day4/data/input"
