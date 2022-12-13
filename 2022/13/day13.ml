
let rec pp_ast ppf = function
  | Ast.Int n -> Format.fprintf ppf "%d" n
  | Ast.List l ->
    let sep ppf () = Format.fprintf ppf "," in
    Format.fprintf ppf "[%a]" (Helper.Pp.list ~sep pp_ast) l


let parse_line s =
  let lexbuf = Lexing.from_string s in
  match Parser.line Lexer.main lexbuf with
  | x -> assert (Format.asprintf "%a" pp_ast x = s); x
  | exception Parser.Error -> Helper.Pp.failf "Failed to parse %s" s

type cmp =
  | Equal
  | Lesser
  | Greater

open Ast
let rec (<<) x y = match x, y with
  | List [] , List (_::_) -> Lesser
  | List [], List [] -> Equal
  | List(_::_), List [] -> Greater
  | Int x , Int y ->
    if x = y then Equal
    else if x < y then Lesser
    else Greater
  | List _ , Int _ -> x << List [y]
  | Int _ , List _ -> List [x] << y
  | List(a::x), List(b::y) ->
    match a << b with
    | Equal -> List x << List y
    | Lesser | Greater as r -> r


let diff (i,sum) l1 l2 =
  let sum =
    match l1 << l2 with
    | Lesser ->
      i + sum
    | Equal -> assert false
    | _ -> sum
  in
  (i+1,sum)

[@@@warning "-32"]
let diff_line isum = function
  | [a;b] -> diff isum (parse_line a) (parse_line b)
  | _ -> assert false
[@@@warning "+32"]

let compare x y = match x << y with
  | Equal -> 0
  | Lesser -> -1
  | Greater -> 1

let acc l x = List.rev_append (List.map parse_line x) l

let () =
  let sep s =  String.trim s = "" in
  let lines = Helper.Input.fold_block sep acc [] "13/data/input" in
  let dividers = List.map Ast.(fun x -> List [List [Int x]]) [2;6] in
  let signal = Array.of_list @@ dividers @ lines in
  let () = Array.sort compare signal in
  let sum =
    let find x =
      match Seq.find_map ( fun (i,y) ->  if x = y then Some (1+i) else None) (Array.to_seqi signal) with
      | Some x -> x
      | _ -> assert false
    in
    List.fold_left (fun s d -> s * find d) 1 dividers in
  Format.printf "key = %d @." sum
