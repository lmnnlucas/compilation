open Util

type type_expr =
  | Type_int
  | Type_float
  | Type_bool
  | Type_pos
  | Type_color
  | Type_point
  | Type_list of type_expr

module Annotation = struct
  type t = { position : Position.t; mutable type_expr : type_expr option }

  let create pos = { position = pos; type_expr = None }
  let get_pos annotation = annotation.position
  let get_type annotation = annotation.type_expr

  let set_type annotation type_expr =
    { annotation with type_expr = Some type_expr }
end

type binary_operator =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | And
  | Or
  | Eq
  | Ne
  | Lt
  | Gt
  | Le
  | Ge

type unary_operator =
  | USub
  | Not
  | Head
  | Tail
  | Floor
  | Float_of_int
  | Cos
  | Sin

type field_accessor =
  | Color_accessor
  | Position_accessor
  | X_accessor
  | Y_accessor
  | Blue_accessor
  | Red_accessor
  | Green_accessor

type expression =
  | Constant_i of int * Annotation.t
  | Constant_f of float * Annotation.t
  | Constant_b of bool * Annotation.t
  | Pos of expression * expression * Annotation.t
  | Color of expression * expression * expression * Annotation.t
  | Point of expression * expression * Annotation.t
  | Variable of string * Annotation.t
  | Binary_operator of binary_operator * expression * expression * Annotation.t
  | Unary_operator of unary_operator * expression * Annotation.t
  | Field_accessor of field_accessor * expression * Annotation.t
  | List of expression list * Annotation.t
  | Cons of expression * expression * Annotation.t

type statement =
  | Assignment of expression * expression * Annotation.t
  | Variable_declaration of string * type_expr * Annotation.t
  | Block of statement list * Annotation.t
  | IfThenElse of expression * statement * statement * Annotation.t
  | For of
      string * expression * expression * expression * statement * Annotation.t
  | Foreach of string * expression * statement * Annotation.t
  | Draw of expression * Annotation.t
  | Nop
  | Print of expression * Annotation.t

type argument = Argument of string * type_expr * Annotation.t
type program = Program of argument list * statement

let rec string_of_type_expr = function
  | Type_int -> "Int"
  | Type_float -> "Float"
  | Type_bool -> "Bool"
  | Type_pos -> "Pos"
  | Type_color -> "Color"
  | Type_point -> "Point"
  | Type_list type_expr -> "List(" ^ string_of_type_expr type_expr ^ ")"

let string_of_binary_operator = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"
  | And -> " And "
  | Or -> " Or "
  | Eq -> " = "
  | Ne -> " <> "
  | Lt -> " < "
  | Gt -> " > "
  | Le -> " <= "
  | Ge -> " >= "

let string_of_unary_operator = function
  | USub -> "-"
  | Not -> " Not"
  | Head -> "Head"
  | Tail -> "Tail"
  | Floor -> "Floor"
  | Float_of_int -> "Float_of_int"
  | Cos -> "Cos"
  | Sin -> "Sin"

let string_of_field_accessor = function
  | Color_accessor -> "Color"
  | Position_accessor -> "Position"
  | X_accessor -> "X"
  | Y_accessor -> "Y"
  | Blue_accessor -> "Blue"
  | Red_accessor -> "Red"
  | Green_accessor -> "Green"

let rec string_of_expression = function
  | Constant_i (value, _) -> string_of_int value
  | Constant_f (value, _) -> string_of_float value
  | Constant_b (value, _) -> string_of_bool value
  | Pos (x, y, _) ->
      "Pos(" ^ string_of_expression x ^ ", " ^ string_of_expression y ^ ")"
  | Color (red, green, blue, _) ->
      "Color(" ^ string_of_expression red ^ ", " ^ string_of_expression green
      ^ ", " ^ string_of_expression blue ^ ")"
  | Point (pos, color, _) ->
      "Point(" ^ string_of_expression pos ^ ", " ^ string_of_expression color
      ^ ")"
  | Binary_operator (op, e1, e2, _) ->
      "(" ^ string_of_expression e1
      ^ string_of_binary_operator op
      ^ string_of_expression e2 ^ ")"
  | Unary_operator (op, e, _) ->
      string_of_unary_operator op ^ "(" ^ string_of_expression e ^ ")"
  | Field_accessor (acc, e, _) ->
      string_of_expression e ^ "." ^ string_of_field_accessor acc
  | Variable (string, _) -> string
  | List (l, _) -> "[" ^ string_of_list l ^ "]"
  | Cons (elt, value, _) ->
      string_of_expression elt ^ "::" ^ string_of_expression value

and string_of_pos = function
  | x, y ->
      "Pos(" ^ string_of_expression x ^ ", " ^ string_of_expression y ^ ")"

and string_of_color = function
  | red, green, blue ->
      "Color(" ^ string_of_expression red ^ ", " ^ string_of_expression green
      ^ ", " ^ string_of_expression blue ^ ")"

and string_of_list = function
  | [] -> ""
  | head :: tail -> (
      match tail with
      | [] -> string_of_expression head
      | _ -> string_of_expression head ^ ", " ^ string_of_list tail)

let rec pp_statement fmt = function
  | Assignment (mutable_expression, expression, _) ->
      Format.fprintf fmt "Copy(%s, %s)"
        (string_of_expression mutable_expression)
        (string_of_expression expression)
  | Variable_declaration (name, type_expr, _) ->
      Format.fprintf fmt "%s(%s)" (string_of_type_expr type_expr) name
  | Block (list_statements, _) ->
      Format.fprintf fmt "Begin@[<v 2>@,%a@]@,End" pp_list_statements
        list_statements
  | IfThenElse (test, statement_then, Nop, _) ->
      Format.fprintf fmt "@[<v 2>If (%s) @,%a@]"
        (string_of_expression test)
        pp_statement statement_then
  | IfThenElse (test, statement_then, statement_else, _) ->
      Format.fprintf fmt "@[<v 2>If (%s)@,%a@]@,@[<v 1>else@,%a@]"
        (string_of_expression test)
        pp_statement statement_then pp_statement statement_else
  | For (name, from_expression, to_expression, step_expression, statement, _) ->
      Format.fprintf fmt "@[<v 2>For %s From %s To %s Step %s@,%a@]" name
        (string_of_expression from_expression)
        (string_of_expression to_expression)
        (string_of_expression step_expression)
        pp_statement statement
  | Foreach (name, expression, statement, _) ->
      Format.fprintf fmt "@[<v 2>Foreach %s In %s@,%a@]" name
        (string_of_expression expression)
        pp_statement statement
  | Draw (expression, _) ->
      Format.fprintf fmt "Draw(%s)" (string_of_expression expression)
  | Nop -> ()
  | Print (expression, _) ->
      Format.fprintf fmt "Print %s" (string_of_expression expression)

and pp_list_statements fmt = function
  | [] -> ()
  | head :: tail -> (
      match tail with
      | [] -> Format.fprintf fmt "%a" pp_statement head
      | _ ->
          Format.fprintf fmt "%a;@,%a" pp_statement head pp_list_statements tail
      )

let pp_argument fmt (Argument (name, typ, _)) =
  Format.fprintf fmt "%s(%s)" (string_of_type_expr typ) name

let pp_program fmt (Program (args, body)) =
  Format.fprintf fmt "@[<v 0>@[<v 2>Arguments <@,";
  List.iteri
    (fun i arg ->
      Format.fprintf fmt "%a%s@," pp_argument arg
        (if i = List.length args - 1 then "" else ";"))
    args;
  Format.fprintf fmt ">@]@,%a@,@]" pp_statement body
