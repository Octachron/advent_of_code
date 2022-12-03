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
