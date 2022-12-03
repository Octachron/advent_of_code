let fold f start filename =
  let rec loop f acc ch =
      match In_channel.input_line ch with
      | Some x -> loop f (f acc x) ch
      | None -> acc
  in
  In_channel.with_open_bin filename (fun ch -> loop f start ch)
