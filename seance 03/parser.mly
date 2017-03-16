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
%token <string> Linteger
%token Ltrue
%token Lfalse

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
     | Linteger                    {Natural (int_of_string $1)}
     | Ltrue                       {Boolean true}
     | Lfalse                      {Boolean false}
     | expr2 expr                  {Application ($1, $2)}

expr2 : expr {$1}

%%
