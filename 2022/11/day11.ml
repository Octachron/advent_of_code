[@@@warning "-32"]

module type worry_algebra = sig
  type t
  val of_int: int -> t
  val (+) : t -> t -> t
  val ( * ) : t -> t -> t
  val divisible: t -> int -> bool
  val pp: Format.formatter -> t -> unit
end

module Worry_algebra(X:sig val lcm: int end): worry_algebra  = struct
  open X
  type t = int
  let of_int x = x mod lcm
  let (+) x y = (x + y) mod lcm
  let ( * ) x y = (x * y) mod lcm
  let divisible x y = (x mod y) = 0
  let pp ppf x = Format.fprintf ppf "%d (mod %d)" x lcm
end
type expr =
  | X
  | Int of int
  | Plus of expr * expr
  | Mult of expr * expr

let rec pp_exp ppf = function
  | X -> Format.fprintf ppf "x"
  | Int n -> Format.fprintf ppf "%d" n
  | Plus (x,y) -> Format.fprintf ppf "%a + %a" pp_exp x pp_exp y
  | Mult (x,y) -> Format.fprintf ppf "%a * %a" pp_exp x pp_exp y

type test =
  | Divisibility of int

type 'a if_then_else = { cond: test; else_: 'a; then_: 'a }

type monkey = {
  id:int;
  update: expr;
  test: int if_then_else
}

module Impl(W:worry_algebra) = struct
  let rec eval x = function
    | X -> x
    | Int n -> W.of_int n
    | Plus (a,b) -> W.( (eval x a) + (eval x b) )
    | Mult (a,b) -> W.( (eval x a) * (eval x b) )

  let test x = function
    | Divisibility n -> W.divisible x n
end

type 'a impl = {
  eval: 'a -> expr -> 'a;
  test :  'a -> test ->  bool;
  decrease: 'a -> 'a
}

let eval_if {test; _ } x t =
  if test x t.cond then t.then_
  else t.else_

let send_to impl monkey obj queues =
  let v = impl.eval obj monkey.update in
  let obj = impl.decrease v in
  let dest = eval_if impl obj monkey.test in
  Queue.add obj queues.(dest)

let rec monkey_round impl queues monkey q nitems =
  if Queue.is_empty q then nitems
  else
    let item = Queue.pop q in
    send_to impl monkey item queues;
    monkey_round impl queues monkey q (nitems + 1)

let pp_queue ppf q = Format.fprintf ppf "(%a)" Helper.Pp.(seq int) (Queue.to_seq q)

let monkey_round worry_decrease queues activities monkey =
  let queue = queues.(monkey.id) in
  let act = monkey_round worry_decrease queues monkey queue activities.(monkey.id) in
  activities.(monkey.id) <- act

let n_items qs =
  Array.fold_left (fun s q -> s + Queue.length q) 0 qs

let round worry_decrease monkeys queues activities =
  let before = n_items queues in
  Array.iter (monkey_round worry_decrease queues activities) monkeys;
  assert (before = n_items queues)

let rec repeat n f = if n = 0 then () else (f (); repeat (n-1) f)

let parse_id s = Scanf.sscanf s "Monkey %d:" (fun d -> d)
let parse_cond s = Scanf.sscanf s " Test: divisible by %d" (fun n -> Divisibility n)
let parse_then s = Scanf.sscanf s " If true: throw to monkey %d" (fun d -> d)
let parse_else s = Scanf.sscanf s " If false: throw to monkey %d" (fun d -> d)
let parse_if_then_else a b c =
  { cond = parse_cond a; then_ = parse_then b; else_ = parse_else c }
let parse_items s =
  let list = Scanf.sscanf s " Starting items: %[0-9, ]%!" (fun s -> s) in
  let list = String.split_on_char ',' list in
  try List.map (fun s -> int_of_string (String.trim s)) list with
  | Failure _ ->
    Helper.Pp.failf "parse items failure: %s | %a" s Helper.Pp.(list ostring) list

module Tokens = struct
  type t =
    | Old
    | P
    | M
    | Lit of int

  let tokenize = function
    | "old" -> Old
    | "+" -> P
    | "*" -> M
    | s ->
      Lit (int_of_string s)
end

module Parse_expr = struct
  open Tokens
  let unary = function
    | Lit n -> Int n
    | Old -> X
    | _ -> assert false
  let rec start = function
    | Lit _ | Old as expr :: rest ->
      right (unary expr) rest
    | _ -> Helper.Pp.failf "Unexpected token: start"
  and right expr = function
    | P :: rest -> plus expr rest
    | M :: rest -> mult expr rest
    | [] -> expr
    | _ -> Helper.Pp.failf "Unexpected token: right"
  and plus expr = function
    | (Lit _ | Old as u) :: q ->
      sum expr (unary u) q
    | _ -> Helper.Pp.failf "Unexpected token: right"
  and mult expr = function
    | (Lit _ | Old as u) :: q ->
      right (Mult(expr, unary u)) q
    | _ -> Helper.Pp.failf "Unexpected token: right"
  and sum left right = function
    | P :: q -> plus (Plus(left,right)) q
    | M :: (Lit _ | Old as u) :: q -> sum left (Mult (right, unary u)) q
    | [] -> Plus(left,right)
    | _ -> Helper.Pp.failf "Unexpected token: right"
end

let parse_operation s =
  let expr = Scanf.sscanf s " Operation: new = %[a-zA-Z 0-9+*]%!" (fun s -> s) in
  let tokens = List.map Tokens.tokenize @@ String.split_on_char ' ' expr in
  Parse_expr.start tokens

let rec parse_entry monkeys = function
  | (""|"\n") :: q -> parse_entry monkeys q
  | [id;items;operation;cond;then_;else_] ->
    let id = parse_id id in
    let items = parse_items items in
    let update = parse_operation operation in
    let test = parse_if_then_else cond then_ else_ in
    (items, { id; update; test }) :: monkeys
  | lines ->
    Helper.Pp.failf "Unexpected format %a" Helper.Pp.(list ostring) lines

let rec gcd a b =
  if b = 0 then a else
  gcd b (a - b * (a/b))

let lcm a b = ( a * b ) / gcd a b
let monkey_div (monkey:monkey) = let Divisibility n = monkey.test.cond in n

let () =
  let items_and_monkeys =
    Array.of_list @@ List.rev @@ Helper.Input.fold_block (fun s -> s = "" || s = "\n") parse_entry [] "11/data/input"
 in
 let monkeys = Array.map snd items_and_monkeys in
 let activities = Array.map (fun _ -> 0) items_and_monkeys in
 let lcm = Array.fold_left (fun m monkey -> lcm m (monkey_div monkey) ) 1 monkeys in
 let module Worry_algebra = Worry_algebra(struct let lcm = lcm end) in
 let module Impl = Impl(Worry_algebra) in
 let impl = { eval=Impl.eval; test = Impl.test; decrease=Fun.id } in
 let qseq (items,_) = Queue.of_seq (Seq.map Worry_algebra.of_int @@ List.to_seq items) in
 let queues = Array.map qseq items_and_monkeys in
 let rounds = 10_000 in
 let () = repeat rounds (fun () -> round impl monkeys queues activities) in
 let () = Array.sort (fun x y -> Stdlib.compare y x) activities in
 let result = activities.(0) * activities.(1) in
 Format.printf "result=%d (%d * %d) | lcm=%d \n" result activities.(0) activities.(1) lcm
