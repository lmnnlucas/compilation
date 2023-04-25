{
    open Parser
    exception Error of string
}

let digit = ['0'-'9']
let alphanum = ['a'-'z' 'A'-'Z' '0'-'9' '\'' '_']

rule token = parse
    
    | "+"               { ADD }
    | "-"               { SUB }
    | "*"               { MUL }
    | "/"               { DIV }
    | "%"               { MOD }
    | "And"             { AND }
    | "Or"              { OR }
    | "="               { EQ }
    | "<>"              { NEQ }
    | "<"               { LT }
    | "<="              { LEQ }
    | ">"               { GT }
    | ">="              { GEQ }
    | "Not"             { NOT }
    | "True"            { BOOL(true) }
    | "False"           { BOOL(false) }
    | "Int"             { INT_TYP }
    | "Float"           { FLOAT_TYP }
    | "If"              { IF }
    | "Else"            { ELSE }
    | "While"           { WHILE }
    | "Do"              { DO }
    | "End"             { END }
    | "Begin"           { BEGIN }
    | "Var"             { VAR }
    | "["               { L_SQ_BRK }
    | "]"               { R_SQ_BRK }
    | "("               { L_PAR }
    | ")"               { R_PAR }
    | "{"               { L_CUR_BRK }
    | "}"               { R_CUR_BRK }
    | ";"               { SEMICOLON }
    | ","               { COMMA }
    | "."               { DOT }
    | "Blue"            { BLUE }
    | "Bool"            { BOOL_TYP }
    | "Color"           { COLOR }
    | "Copy"            { COPY }
    | "Cos"             { COS }
    | "Draw"            { DRAW }
    | "Float_of_int"    { FLOAT_OF_INT }
    | "Floor"           { FLOOR }
    | "For"             { FOR }
    | "Foreach"         { FOREACH }
    | "From"            { FROM }
    | "Green"           { GREEN }
    | "Head"            { HEAD }
    | "In"              { IN }
    | "List"            { LIST }
    | "Point"           { POINT }
    | "Pos"             { POS }
    | "Print"           { PRINT }
    | "Red"             { RED }
    | "Sin"             { SIN }
    | "Step"            { STEP }
    | "Tail"            { TAIL }
    | "To"              { TO }
    | "X"               { X }
    | "Y"               { Y }
    | "::"              { CONS }
    | "Do"              { DO }

    | "\"" ([^ '\"']* as s) "\""  { STRING(s) }
    | (digit)* "." (digit)* as s {FLOAT(try float_of_string s with Failure _ -> raise (Error(s)) )}
    | (digit)+ as s     { INT(try int_of_string s with Failure _ ->(let pos = Lexing.lexeme_start_p lexbuf in raise (Error(Format.sprintf "Line %d, char %d ,Read: '%s'. It is not a valid integer" pos.pos_lnum (pos.pos_cnum - pos.pos_bol +1) s)) ))}
    | ['a'-'z' 'A'-'Z'] (alphanum)* as s  { ID(s) }
    | _ as s            { let pos = Lexing.lexeme_start_p lexbuf in raise (Error(Format.sprintf "Line %d, char %d ,Read: '%c'. It is not an acceptable character" pos.pos_lnum (pos.pos_cnum - pos.pos_bol +1) s)) }
    | eof               { EOF }

and commentary = parse
    | '\n'      {Lexing.new_line lexbuf; commentary lexbuf}
    | "*/"      { token lexbuf }
    | _ { commentary lexbuf }