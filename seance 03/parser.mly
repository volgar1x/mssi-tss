%{
  open Types ;;
%}

%token Leol
%token Loparen
%token Lcparen
%token Llambda
%token Ldot
%token Lglobal
%token Leq
%token <string> Lident
%token <string> Linteger
%token Ltrue
%token Lfalse
%token Lif
%token Lthen
%token Lelse
%token Llet
%token Lin
%token Lend

%start line
%type <Types.expression> line

%%

line :
     | expr Leol                           {$1}
;

atom :
    | Lident                              {Variable ($1)}
    | Linteger                            {Natural (int_of_string $1)}
    | Ltrue                               {Boolean true}
    | Lfalse                              {Boolean false}
;

expr :
     | Loparen expr Lcparen                {$2}
     | expr atom                           {Application ($1, $2)}
     | Llambda Lident Ldot expr            {Function ($2, $4, [])}
     | Lif expr Lthen expr Lelse expr      {Cond ($2, $4, $6)}
     | Llet Lident Leq expr Lin expr       {Bind ($2, $4, $6)}
     | atom                                {$1}
;

finish_let :
  | Lend     {Unit}
  | Lin expr {$2}
;

%%
