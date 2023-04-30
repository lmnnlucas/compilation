%{
    open Ast

    (*Le parseur doit générer des Ast.program.
    Pour les annotation, il vous suffit de créer l’annotation avec [Annotation.create $loc] qui sera ce qu’il vous faut dans tous les cas : $loc est un paramètre qui correspond aux positions délimitant la chaîne de caractères parsée par la règle où il est utilisé, ce qui est exactement ce qu’il faut mettre dans l’annotation.*)
%}

%token BEGIN END

%token IF
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
%token <int> INT
%token <float> FLOAT
%token <bool> BOOL

%token EOF

%right IF
%nonassoc ELSE

%left AND OR
%left EQ NEQ LT GT LEQ GEQ
%left ADD SUB 
%left MUL DIV MOD
%left DOT
%right CONS
%nonassoc NOT HEAD TAIL FLOOR COS SIN FLOAT_OF_INT

%token BLUE RED GREEN

%start <program> main
%%

main:
| LT argument_list GT statement EOF             { Program (List.rev $2, $4) }
| statement EOF                                 { Program ([], $1) }
| EOF                                           { Program([],Block([],Annotation.create $loc)) }

argument:
| type_basic L_PAR ID R_PAR                      { Argument ($3, $1, Annotation.create $loc) }
| LIST L_PAR type_basic R_PAR L_PAR ID R_PAR     { Argument ($6, $3, Annotation.create $loc) }

argument_list:
| argument { [$1] }
| argument_list SEMICOLON argument { $3::$1 }  
| { [] }

type_basic:
| INT_TYP { Type_int }
| FLOAT_TYP { Type_float }
| BOOL_TYP { Type_bool }
| POS { Type_pos }
| COLOR { Type_color }
| POINT { Type_point }

statement:
| COPY L_PAR expression COMMA expression R_PAR                      { Assignment ($3, $5, Annotation.create $loc) }
| LIST L_PAR type_basic R_PAR L_PAR ID R_PAR                        { Variable_declaration ($6, Type_list($3), Annotation.create $loc) }
| type_basic L_PAR ID R_PAR                                         { Variable_declaration ($3, $1, Annotation.create $loc) }
| BEGIN statement_list END                                          { Block(List.rev $2, Annotation.create $loc) }
| IF expression statement ELSE statement                            { IfThenElse( $2, $3, $5, Annotation.create $loc) }
| IF expression statement                                           { IfThenElse ($2, $3, Nop, Annotation.create $loc)}
| FOR ID FROM expression TO expression STEP expression statement    { For ($2, $4, $6, $8, $9, Annotation.create $loc) }
| FOREACH ID IN expression statement                                { Foreach ($2, $4, $5, Annotation.create $loc) }
| DRAW L_PAR expression R_PAR                                       { Draw ($3, Annotation.create $loc) }
| PRINT expression                                                  { Print ($2, Annotation.create $loc) }  

statement_list:
| statement { [$1] }
| statement_list SEMICOLON statement { $3::$1 }  
| { [] }

expression:
| INT                                                                   { Constant_i ($1, Annotation.create $loc) }
| FLOAT                                                                 { Constant_f ($1, Annotation.create $loc) }
| BOOL                                                                  { Constant_b ($1, Annotation.create $loc) }
| POS L_PAR expression COMMA expression R_PAR                           { Pos ($3, $5, Annotation.create $loc) }
| COLOR L_PAR expression COMMA expression COMMA expression R_PAR        { Color ($3, $5, $7, Annotation.create $loc) }
| POINT L_PAR expression COMMA expression R_PAR                         { Point ($3, $5, Annotation.create $loc) }
| ID                                                                    { Variable ($1, Annotation.create $loc) }
| expression binop expression                                           { Binary_operator ($2, $1, $3, Annotation.create $loc)}
| unop expression                                                       { Unary_operator ($1, $2, Annotation.create $loc)}
| expression DOT field_accessor                                         { Field_accessor($3, $1, Annotation.create $loc) }
| expression CONS expression                                            { Cons ($1, $3, Annotation.create $loc)}
| L_SQ_BRK expression_list R_SQ_BRK                                     { List (List.rev $2, Annotation.create $loc)}
| L_PAR expression R_PAR                                                { $2 }

field_accessor:
| X     { X_accessor }
| Y     { Y_accessor }
| GREEN { Green_accessor }
| RED   { Red_accessor }
| BLUE  { Blue_accessor }
| COLOR { Color_accessor }
| POS   { Position_accessor }

expression_list:
| expression { [$1] }
| expression_list COMMA expression { $3::$1 }  
| { [] }

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
| SUB               { USub }
| NOT               { Not }
| HEAD              { Head }
| TAIL              { Tail }
| FLOOR             { Floor }
| FLOAT_OF_INT      { Float_of_int }
| COS               { Cos }
| SIN               { Sin }