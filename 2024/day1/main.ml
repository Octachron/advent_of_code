
let parse_line x =
  match Scanf.sscanf_opt x "%d %d" (fun x y -> x, y) with
  | Some x -> x
  | None -> Format.eprintf "Error when scanning %s@." x; exit 2

let parse filename =
  List.rev @@ In_channel.with_open_text filename
    (In_channel.fold_lines (fun l x -> (parse_line x)::l) [])

module[@warning "-32"] Problem_one = struct
  let match_lr l =
    let l, r = List.split l in
    let sort l = List.to_seq @@ List.sort Stdlib.compare l in
    let c = Seq.zip (sort l) (sort r) in
    Seq.fold_left (fun sum (l,r) ->
        sum + abs (r-l)
      ) 0 c

  let main () =
    let filename = Sys.argv.(1) in
    filename |> parse |> match_lr |> Format.printf "Distance = %d@."
end

module Problem_two = struct
  module Int_map = Map.Make(Int)
  let (.%()) m x = match Int_map.find x m with
    | exception Not_found -> 0
    | x -> x

  let add m x =
    Int_map.add x (1 + m.%(x)) m

  let multiset l = List.fold_left add Int_map.empty l

  let count l =
    let l, r = List.split l in
    let ms = multiset r in
    List.fold_left (fun score x -> score + x * ms.%(x)) 0 l

  let main () =
    let filename = Sys.argv.(1) in
    filename |> parse |> count |> Format.printf "Similarité %d@."
end

let () = Problem_two.main ()
