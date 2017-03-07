%{
  open Types ;;
%}

%token Leol
%token Loparen
%token Lcparen
%token Llambda
%token Ldot
%token Llet
%token Leq
%token <string> Lident

%start line
%type <Types.expression> line

%%

line :
     | expr Leol                   {$1}

expr :
     | Loparen expr Lcparen        {$2}
     | Llet expr Leq expr          {Assignation ($2, $4)}
     | Llambda Lident Ldot expr    {Function ($2, $4, [])}
     | Lident                      {Variable ($1)}
     | expr2 expr                  {Application ($1, $2)}

expr2 : expr {$1}

%%
