let number = function
  | '0' .. '9' as c -> Some (Char.code c - Char.code '0')
  | _ -> None

module Part_one = struct
  [@@@warning "-32"]
  let digits x =
    x |> String.to_seq |> Seq.filter_map number |> List.of_seq
end
[@@@warnings "+32"]

let names = List.mapi (fun i x -> x, 1 + i) [
  "one";
  "two";
  "three";
  "four";
  "five";
  "six";
  "seven";
  "eight";
  "nine";
]

let rec slice_eq (x,p1) (y,p2) =
  if p1 >= String.length x then true
  else if p2 >= String.length y then false
  else x.[p1] = y.[p2] && slice_eq (x,1+p1) (y,1+p2)

let str_slice_eq str slice =
  slice_eq (str,0) slice

let match_str s pos (str,_) =
  str_slice_eq str (s,pos)

let rec tokenize s len pos =
  if pos >= len then []
  else match s.[pos] with
    | '0' .. '9' as c ->
      (Char.code c - Char.code '0') :: tokenize s len (pos+1)
    | _ ->
      match List.find_opt (match_str s pos) names with
      | Some (_,n) -> n :: tokenize s len (pos + 1)
      | None -> tokenize s len (pos+1)

let alphadigits s = tokenize s (String.length s) 0

let read tokenizer f = Helper.Input.fold (fun acc line ->
    let digits = tokenizer line in
    let n =
      match digits, List.rev digits with
      | x :: _, y :: _ -> 10 * x + y
      | [], _ | _, [] ->
        Format.eprintf "No numbers? %S@." line;
        assert false
    in
    n + acc)
    0 f


let () =
  let sum = read alphadigits "2023/day1/data/input" in
  Format.printf "@[sum=%a@]@." Helper.Pp.int sum
