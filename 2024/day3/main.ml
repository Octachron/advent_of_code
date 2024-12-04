let[@warning "-32"] rec parse b =
  if Scanf.Scanning.end_of_input b then 0 else
    match Scanf.bscanf_opt b "mul(%d,%d)" (fun x y -> x * y) with
    | None -> Scanf.bscanf b "%_c" (); parse b
    | Some x -> x + parse b


let skip b = Scanf.bscanf b "%_c" ()

let rec parse2 enabled sum b =
  if Scanf.Scanning.end_of_input b then sum else
    let next () = skip b; parse2 enabled sum b in
    match Scanf.bscanf_opt b "mul(%d,%d)" (fun x y -> x * y) with
    | Some x ->
      let sum = if enabled then x + sum else sum in
      parse2 enabled sum b
    | None ->
      match Scanf.bscanf_opt b "do" () with
      | Some () ->
        begin match Scanf.bscanf_opt b "()" () with
          | Some () -> parse2 true sum b
          | None -> match Scanf.bscanf_opt b "n't()" () with
              | Some () -> parse2 false sum b
              | None -> next ()
        end
      | None -> next ()

let parse filename =
  let b = Scanf.Scanning.from_file filename in
  parse2 true 0 b

let () = Format.printf "result %d@." (parse Sys.argv.(1))
