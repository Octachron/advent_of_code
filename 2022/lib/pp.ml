let none _ppf () = ()
let comma ppf () = Format.fprintf ppf ",@ "
let int ppf = Format.fprintf ppf "%d"
let char ppf = Format.fprintf ppf "%c"

let list ?(sep=comma) elt ppf x = Format.pp_print_list ~pp_sep:sep elt ppf x
let array ?(sep=comma) elt ppf x = Format.pp_print_seq ~pp_sep:sep elt ppf (Array.to_seq x)
let failf fmt = Format.kfprintf (fun ppf -> Format.fprintf ppf "@."; exit 2) Format.err_formatter fmt
