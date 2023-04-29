%{
    open Ast

    (*Le parseur doit générer des Ast.program.
    Pour les annotation, il vous suffit de créer l’annotation avec [Annotation.create $loc] qui sera ce qu’il vous faut dans tous les cas : $loc est un paramètre qui correspond aux positions délimitant la chaîne de caractères parsée par la règle où il est utilisé, ce qui est exactement ce qu’il faut mettre dans l’annotation.*)
%}

%token BEGIN END

%token IF
%token THEN
%token ELSE
%token FOR
%token FOREACH
%token FROM
%token L_PAR
%token R_PAR
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
%token INT_TYP
%token FLOAT_TYP
%token BOOL_TYP
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
%nonassoc NOT HEAD TAIL FLOOR COS SIN FLOAT_OF_INT

%token BLUE RED GREEN

%start <program> main
%%

main:
| BEGIN statement_list END EOF                  { Program (List.rev $1) }
| BEGIN statement END                           { Statement $1 }
| LT statement_list GT                          {}
| statement  EOF                                { Statement $1 }
| EOF                                           { Program([],Block([],Annotation.create $loc)) }

statement_list:
| statement { [$1] }
| statement_list SEMICOLON statement { [$3::$1] }  
| { [] }

// expression_list:

type_basic:
| INT_TYP { Type_int }
| FLOAT_TYP { Type_float }
| BOOL_TYP { Type_bool }

statement:
| COPY L_PAR ID COMMA expression R_PAR SEMICOLON                    { Assignement ($3 $5 Annotation.create $loc) }
| type_basic L_PAR STRING R_PAR SEMICOLON                           { Variable_declaration ($3 $1 Annotation.create $loc) }
| BEGIN statement_list END                                          { Block([$2], Annotation.create $loc) }
| IF expression THEN statement ELSE statement                       { IfThenElse( $2 $4 $6 Annotation.create $loc) }
| IF expression THEN statement                                      { IfThenElse ($2 $4 Nop)}
| FOR ID FROM expression TO expression STEP expression statement    { For ($2 $4 $6 $7 Annotation.create $loc) }
| FOREACH ID IN expression statement                                { Foreach ($2 $4 $5 Annotation.create $loc) }
| DRAW L_PAR expression R_PAR SEMICOLON                             { Draw ($3 Annotation.create $loc) }
| PRINT expression SEMICOLON                                        { PRINT ($2 Annotation.create $loc) }  

expression:
| INT                                                                   { Constant_i ($1 Annotation.create $loc) }
| FLOAT                                                                 { Constant_f ($1 Annotation.create $loc) }
| BOOL                                                                  { Constant_b ($1 Annotation.create $loc) }
| POS L_PAR expression COMMA expression R_PAR                           { Pos ($3 $5 Annotation.create $loc) }
| COLOR L_PAR expression COMMA expression COMMA expression R_PAR        { Color ($3 $5 $7 Annotation.create $loc) }
| POINT L_PAR expression COMMA expression R_PAR                         { Point ($3 $5 Annotation.create $loc) }
| ID                                                                    { Variable ($1 Annotation.create $loc) }
| expression binop expression                                           { Binary_operator ($2 $1 $3 Annotation.create $loc)}
| unop expression                                                       { Unary_operator ($1 $2 Annotation.create $loc)}
| ID DOT X                                                              { Field_accessor( X_accessor $1 Annotation.create $loc) }
| ID DOT Y                                                              { Field_accessor (Y_accessor $1 Annotation.create $loc) } 
| ID DOT GREEN                                                          { Field_accessor (Green_accessor $1 Annotation.create $loc) }
| ID DOT RED                                                            { Field_accessor (Red_accessor $1 Annotation.create $loc) }
| ID DOT BLUE                                                           { Field_accessor (Blue_accessor $1 Annotation.create $loc) }
| ID DOT COLOR                                                          { Field_accessor (Color_accessor $1 Annotation.create $loc) }
| ID DOT POS                                                            { Field_accessor (Position_accessor $1 Annotation.create $loc) }

%inline binop:
| ADD   { Add }
| SUB   { Sub }
| MUL   { Mul }
| DIV   { Div }
| MOD   { Mod }
| AND   { And }
| OR    { Or }
| EQ    { Eq }
| NEQ   { Ne }
| LT    { Lt }
| GT    { Gt }
| LEQ   { Le }
| GEQ   { Ge }

%inline unop:
| SUB   { USub }
| NOT   { Not }
| HEAD  { Head }
| TAIL  { Tail }
| FLOOR { Floor }
| FLOAT_OF_INT { Float_of_int }
| COS { Cos }
| SIN { Sin }