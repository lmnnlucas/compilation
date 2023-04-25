%{
    open Ast

    (*Le parseur doit générer des Ast.program.
    Pour les annotation, il vous suffit de créer l’annotation avec [Annotation.create $loc] qui sera ce qu’il vous faut dans tous les cas : $loc est un paramètre qui correspond aux positions délimitant la chaîne de caractères parsée par la règle où il est utilisé, ce qui est exactement ce qu’il faut mettre dans l’annotation.*)
%}

%token BEGIN END

%token IF
%token THEN
%token ELSE
%token DO
%token WHILE
%token FOR
%token FOREACH
%token FROM
%token L_PAR
%token R_PAR
%token L_CUR_BRK
%token R_CUR_BRK
%token L_SQ_BRK
%token R_SQ_BRK
%token ADD
%token SUB
%token MUL
%token DIV
%token MOD
%token AND
%token OR
%token NOT
%token EQ
%token NEQ
%token LT
%token GT
%token LEQ
%token GEQ
%token COMMA
%token SEMICOLON
%token DOT
%token PRINT
%token RETURN
%token INT_TYP
%token FLOAT_TYP
%token BOOL_TYP
%token NULL_TYP
%token VAR
%token COLOR
%token COPY
%token COS
%token DRAW
%token HEAD
%token IN
%token LIST
%token FLOAT_OF_INT
%token FLOOR
%token POINT
%token POS
%token SIN
%token STEP
%token TAIL
%token TO
%token X 
%token Y
%token CONS

%token <string> ID
%token <string> STRING
%token <int> INT
%token <float> FLOAT
%token <bool> BOOL

%token EOF

%nonassoc ELSE
%left AND OR
%left EQ NEQ LT GT LEQ GEQ
%left ADD SUB 
%left MUL DIV MOD
%nonassoc NOT

%token BLUE RED GREEN

%start <program> main
%%

main:
| EOF { Program([],Block([],Annotation.create $loc)) }
