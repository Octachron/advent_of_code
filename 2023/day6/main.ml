let input = [
  41, 214;
  96, 1789;
  88, 1127;
  94,  1055
]


(** distance: a * (n - a) = max + 1 *)
(** a² - n a + (1+mx) *)
(** delta = sqrt (n² - 4 n * ( 1 + mx )) / 2 *)

type g = { time:int; record:int }

let input = List.map (fun (time,record) -> {time;record}) input

let delta {time; record=mx} =
  let mxp = 1 + mx in
  let delta_2 = time * time - 4 * mxp in
  (sqrt (float delta_2))

let pp_g ppf g = Format.fprintf ppf "{time=%d;@ record=%d}" g.time g.record


let check g x =
  let value =
  x * (g.time - x) in
  value > g.record ||
  begin
    Format.eprintf "@[Failure: %d for game %a: %d <= %d@]@."
      x pp_g g value g.record;
    false
  end

let score g =
  let delta = delta g  in
  Format.eprintf "delta=%g@." delta;
  let d = delta /. 2. in
  let mid = float g.time /. 2. in
  let xp = int_of_float @@ floor (mid +. d) in
  let xm = int_of_float @@ ceil (mid -. d) in
  assert (check g xm);
  assert (check g xp);
  assert (not (check g (xm-1)));
  assert (not (check g (xp+1)));
  let d = int_of_float @@ floor delta in
  let d' = 1 + xp - xm in
  Format.eprintf "%d|%d?@." d d';
  max d d'

let total_score = List.fold_left (fun acc x -> acc * score x) 1 input

let () = Format.printf "total_score part 1 = %d@." total_score

let input2 =
  { time= 41_96_88_94;
    record = 214_1789_1127_1055
  }

let () = Format.printf "score part 2 = %d@." (score input2)
