let none _ppf () = ()
let comma ppf () = Format.fprintf ppf ",@ "
let break ppf () = Format.fprintf ppf "@,"

let int ppf = Format.fprintf ppf "%d"
let char ppf = Format.fprintf ppf "%c"
let ostring ppf s = Format.fprintf ppf "%S" s

let seq ?(sep=comma) elt ppf x = Format.pp_print_seq ~pp_sep:sep elt ppf x
let list ?(sep=comma) elt ppf x = Format.pp_print_list ~pp_sep:sep elt ppf x
let array ?(sep=comma) elt ppf x = Format.pp_print_seq ~pp_sep:sep elt ppf (Array.to_seq x)
let failf fmt = Format.kfprintf (fun ppf -> Format.fprintf ppf "@."; exit 2) Format.err_formatter fmt
