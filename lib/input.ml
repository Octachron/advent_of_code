let fold f start filename =
  let rec loop f acc ch =
      match In_channel.input_line ch with
      | Some x -> loop f (f acc x) ch
      | None -> acc
  in
  In_channel.with_open_bin filename (fun ch -> loop f start ch)

let group_by k f start filename =
  let init state = (0, Array.make k "", state) in
  let folder (counter, array, inner) x =
    let () = array.(counter) <- x in
    let counter = succ counter in
    if counter = k then
      init (f inner array)
    else
      (counter, array, inner)
  in
  let _,_, result = fold folder (init start) filename in
  result


let fold_block sep f start filename =
  let g (line_acc, acc) s =
    if sep s then
      let block = List.rev line_acc in
      ([],f acc block)
    else
      (s :: line_acc, acc)
  in
  let lines, acc = fold g ([],start) filename in
  match lines with
  | [] -> acc
  | some -> f acc (List.rev some)

let rec num_list is_sep bf =
  if Scanf.Scanning.end_of_input bf || Scanf.bscanf bf "%0c" is_sep then [] else
  Scanf.bscanf bf "%d %r" (num_list is_sep) (fun s l -> s :: l )
