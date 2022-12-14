%token ARROW
%token EOF
%token COMMA
%token<int> INT

%{%}
%start <Helper.Vec2.t list> line
%%

line:
  | l = separated_nonempty_list(ARROW, coordinate) EOF { l }

coordinate:
| x=INT COMMA y=INT {Helper.Vec2.make x y}
