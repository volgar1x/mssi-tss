%{
  open Types ;;
%}

%token Leol
%token Ltrue
%token Lfalse
%token Lif
%token Lthen
%token Lelse
%token Lzero
%token Lsucc
%token Lpred
%token Liszero

%start line                       /* axiome */
%type <Types.expression> line     /* type de l'attribut de l'axiome */  

%%

line :
    | expr Leol            {$1}

expr :
    | Ltrue                             {True}
    | Lfalse                            {False}
    | Lif expr Lthen expr Lelse expr    {IfElse ($2, $4, $6)}
    | Lzero                             {Zero}
    | Lsucc expr                        {Succ $2}
    | Lpred expr                        {Pred $2}
    | Liszero expr                      {IsZero $2}

%%
