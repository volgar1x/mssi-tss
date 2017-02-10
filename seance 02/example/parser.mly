%{
  open Types ;;
%}

%token Leol
%token Llpar
%token Lrpar
%token Lnot
%token Linvolve
%token Lequiv
%token Land
%token Lor
%token Ltrue
%token Lfalse
%token <string> Lident            /* type de l'attribut fourni par le lexer */

%left Lequiv                      /* gestion des règles d'associativité */
%left Linvolve                    /* et de priorité permettant de lever */  
%left Lor                         /* les conflits dûs à l'ambiguïté de  */ 
%left Land                        /* la grammaire */
%right Lnot

%start line                       /* axiome */
%type <Types.proposition> line    /* type de l'attribut de l'axiome */  

%%

line :
  | prop Leol            {$1}

prop :
  | prop Lequiv prop     {Equiv ($1, $3)}
  | prop Linvolve prop   {Involve ($1, $3)}
  | prop Lor prop        {Or ($1, $3)}
  | prop Land prop       {And ($1, $3)}
  | Lnot prop            {Not $2}
  | Llpar prop Lrpar     {$2}
  | Ltrue                {True}
  | Lfalse               {False}
  | Lident               {Var $1}

%%
