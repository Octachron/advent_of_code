rule main = parse
| ',' {Parser.COMMA}
| "->" { Parser.ARROW }
| ['0'-'9']+ as n { Parser.INT (int_of_string n) }
| [' ''\n''\r']+ { main lexbuf }
| eof { EOF }
