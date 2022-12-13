{ open Parser }
rule main= parse
 | ',' { COMMA }
 | ['0'-'9']+ as s { INT (int_of_string s) }
 | '[' { OPEN }
 | ']' { CLOSE }
 | ['\n'' ''\t''\r'] { main lexbuf }
 | eof { EOF }
