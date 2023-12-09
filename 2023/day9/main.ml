
type derived = { mutable order:int; points: int array }

let init points = { order=0; points }

let iteri d f =
  for i = d.order to Array.length d.points - 1 do
    f i d.points.(i)
  done


let rev_iteri d f =
  for i = Array.length d.points - 1 downto d.order  do
    f i d.points.(i)
  done

let derive d =
  d.order <- d.order + 1;
  rev_iteri d (fun i x ->
    d.points.(i) <- x - d.points.(i-1)
    );
  d

let const d =
  if d.order > Array.length d.points then true
  else
   let exception Stop in
   try
     iteri d (fun _ x ->
         if x <> d.points.(d.order) then raise Stop
       );
     true
   with Stop -> false

let integrate d =
  assert (d.order > 0 );
  iteri d (fun i x ->
      d.points.(i) <- x + d.points.(i - 1)
    );
  d.order <- d.order - 1;
  d

let rec integrate_full d =
  if d.order = 0 then d else begin
    integrate_full (integrate d)
  end

let extend d =
  let points =
    Array.init (1 + Array.length d.points) (fun i ->
        if i < Array.length d.points then d.points.(i)
        else d.points.(d.order) )
  in
  { d with points }


let rec new_point x =
  if const x then
    let y = extend x in
    integrate_full y
  else
    x |> derive |> new_point

let pp ppf x =
  Helper.Pp.(seq int) ppf (Array.to_seq x.points)

let parse_line = Helper.Input.num_list (Fun.const false)

let rec compute_backward pos c d =
  if pos = -1 then c
  else
    let c = d.points.(pos) - c in
    compute_backward (pos - 1) c d

let rec backward d =
  if const d then
    compute_backward (d.order - 1) d.points.(d.order) d
  else
    backward (derive d)

let[@warning "-32"] fold_part1 acc x =
  let points = init @@ Scanf.sscanf x "%r" parse_line Array.of_list in
  Format.eprintf "@[<v>before:@[%a@]@," pp points;
  let np = points |> new_point in
  Format.eprintf "after :@[%a@]@]@." pp np;
  let last = np.points.(Array.length np.points - 1) in
  acc + last

let fold_part2 acc x =
  let points = init @@ Scanf.sscanf x "%r" parse_line Array.of_list in
  Format.eprintf "@[<v>points:@[%a@]@," pp points;
  let first = backward points in
  Format.eprintf "before :%d @]@." first;
  acc + first


let main data =
  let score = Helper.Input.fold fold_part2 0 data in
  Format.eprintf "score = %d@." score

let () = main "2023/day9/data/input"
