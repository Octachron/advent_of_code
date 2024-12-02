
let parse_line l = String.split_on_char ' ' l |> List.map int_of_string


let diff l = match l with
  | [] -> assert false
  | a :: rest ->
    let _, l = List.fold_left (fun (prev,l) x -> x, (x - prev)::l ) (a,[]) rest in
    List.rev l

let rec all_lists = function
  | [] -> []
  | a :: q -> q :: List.map (List.cons a) (all_lists q)

let safe l =
  (List.for_all (fun x -> x > 0) l || List.for_all (fun x -> x<0) l)
  && List.for_all (fun x -> abs x <= 3) l

let[@warning "-32"] score prev line =
  if (line |> parse_line |> diff |> safe) then 1 + prev else prev

let score2 prev line =
  if (line |> parse_line |> all_lists |> List.map diff |> List.exists safe) then 1 + prev else prev


let main score =
  let result =
    In_channel.with_open_text Sys.argv.(1) (fun chan ->
        In_channel.fold_lines score 0 chan
      )
  in
  Format.printf "%d safe lines@." result

let () = main score2
