%token<int> INT
%token OPEN
%token EOF
%token COMMA
%token CLOSE
%start<Ast.t> line
%{%}
%%
line:
  l=alist EOF { l }

alist:
 | OPEN l=separated_list(COMMA,alist) CLOSE { Ast.List l }
 | a=atom { a }

atom:
| i=INT { Ast.Int i }
