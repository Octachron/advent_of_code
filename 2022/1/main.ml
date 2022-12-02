type token =
  | End_elf
  | Calories of int

let parse_int s =
  match int_of_string s with
  | x -> x
  | exception Failure _ ->
    Format.eprintf "Failed to parse %S@." s;
    exit 2

let parse = function
  | " " | "" -> End_elf
  | s -> Calories (parse_int s)

let read_line ch =
  Option.map parse (In_channel.input_line ch)

module Int_set = Set.Make(Int)

let add_if_greater set n =
  let mn = Int_set.min_elt set in
  if n > mn then
    set |> Int_set.remove mn |> Int_set.add n
  else
    set

let rec read_ch mx_set current_elf ch =
  match read_line ch with
  | Some End_elf ->
    let mx_set = add_if_greater mx_set current_elf in
    read_ch mx_set 0 ch
  | Some (Calories n) ->
    read_ch mx_set (current_elf + n) ch
  | None -> mx_set

let read file seed =
  In_channel.with_open_bin file (read_ch (Int_set.of_list seed) 0)

module Pp = struct
  let seq = Format.pp_print_seq
  let int ppf x = Format.fprintf ppf "%d" x
  let comma ppf () = Format.fprintf ppf ",@ "
  let int_set ppf s = Format.fprintf ppf "{%a}"
    (seq ~pp_sep:comma int) (Int_set.to_seq s)
end

let () =
  let set = read "2022/1/data/input" [-1;-2;-3] in
  Format.printf "@[max=%a@]@." Pp.int_set set;
  let total = Int_set.fold (+) set 0 in
  Format.printf "@[total=%d@]@." total
