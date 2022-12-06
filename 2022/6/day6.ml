module Buffer: sig
  type !'a t
  val len: 'a t -> int
  val make: 'a array -> 'a t
  val add:'a t -> 'a -> 'a t
  val (.!()): 'a t -> int -> 'a
  val pp: (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end = struct
  type 'a t = { mutable pos: int; buffer:'a array}
  let len b = Array.length b.buffer
  let make x = { pos = 0; buffer = x }
  let (mod) x b = x mod len b
  let add b x =
    b.buffer.(b.pos) <- x;
    b.pos <- (b.pos + 1) mod b;
    b
  let (.!()) b n =
    b.buffer.(b.pos + n mod b)
  let pp elt ppf b =
    Format.fprintf ppf "[%a]"
    Helper.Pp.(array elt) b.buffer
end

module Char_map = Map.Make(Char)

let incr c map = match Char_map.find_opt c map with
  | None -> Char_map.add c 1 map
  | Some n -> Char_map.add c (1+n) map

let decr c map = match Char_map.find_opt c map with
  | None -> assert false
  | Some n ->
    if n = 1 then
      Char_map.remove c map
    else
      Char_map.add c (n-1) map

type ('a,'b) status =
  | Done of 'a
  | On_going of 'b


module Pp = Helper.Pp

let pp_count ppf (c,x) = Format.fprintf ppf "%c(%d)" c x

let _pp ppf (map,buffer) =
  Format.fprintf ppf "@[%a|%a@]"
    (Buffer.pp Pp.char) buffer
    (Pp.list pp_count) (Char_map.bindings map)


let advance map buffer c =
  let last = buffer.Buffer.!(0) in
  let map = map |> decr last |> incr c in
  let buffer = Buffer.add buffer c in
  if Char_map.cardinal map = Buffer.len buffer then begin
    Done ()
  end
  else
    On_going (map,buffer)

let rec search s map buffer pos =
  if pos > String.length s then
    On_going (map,buffer)
  else match advance map buffer s.[pos] with
    | Done () -> Done pos
    | On_going (map,buffer) ->
      search s map buffer (pos+1)

let search_in n s =
  let l = List.init n (fun i -> s.[i]) in
  let buffer = Buffer.make (Array.of_list l) in
  let char_map =
    List.fold_left (fun map c -> incr c map) Char_map.empty l
  in
  search s char_map buffer 4

let folder n status s = match status with
  | Done pos -> Done pos
  | On_going () ->
    match search_in n s with
    | On_going _ -> On_going ()
    | Done pos -> Done pos

let () =
  let res =
    Helper.Input.fold (folder 14) (On_going ())
      "6/data/input"
  in
  match res with
  | On_going _ -> assert false
  | Done pos ->
    Format.printf "@.@[start=%d@]@." (1+pos)
