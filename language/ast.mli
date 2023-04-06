(** Module that represent programs in the Stippled language. A program is composed of a list of argument, followed by a statement that is the body of the program. Statements contain expressions. Arguments, statements and expressions contain annotations (their position and types).
    
This modules contains also a set of printer functions.*)

(** Types of the language*)
type type_expr =
  | Type_int
  | Type_float
  | Type_bool
  | Type_pos
  | Type_color
  | Type_point
  | Type_list of type_expr

module Annotation : sig
  (** Module representing annotations of elements of the program. It contains the {!Util.Position.t} of the parsed text that represent the element, and its {!type-type_expr}. The type is only useful for expressions. *)

  type t
  (** The Annotation type.*)

  val create : Util.Position.t -> t
  (** [create pos] creates an {!t} with the position set as [pos] and no type set.
      To be used within the parser (when creating the Ast), and if new expressions/instructions are created during the analysis phase (notably if implementing the implicit cast).*)

  val get_pos : t -> Util.Position.t
  (** [get_pos annotation] return the position within [annotation].*)

  val get_type : t -> type_expr option
  (** [get_type annotation] returns [Some t] if [t] is the {!type-type_expr} within the annotation, and [None] if none is set.*)

  val set_type : t -> type_expr -> t
  (** [set_type annotation t] sets the type within [annotation] to be [t]*)
end

(** Binary operators over {!expression}. Operations over {!Type_pos}, {!Type_color} and {!Type_point} are applied member by member (i.e., (a,b) + (c,d) * (a+c,b+d)).*)
type binary_operator =
  | Add
      (** Addition over {!Type_int}, {!Type_float}, {!Type_pos}, {!Type_color} and {!Type_point}, and concatenation over {!Type_list}*)
  | Sub
      (** Substraction over {!Type_int}, {!Type_float}, {!Type_pos}, {!Type_color} and {!Type_point}*)
  | Mul
      (** Multiplication over {!Type_int}, {!Type_float}, {!Type_pos}, {!Type_color} and {!Type_point}*)
  | Div
      (** Division over {!Type_int}, {!Type_float}, {!Type_pos}, {!Type_color} and {!Type_point}*)
  | Mod
      (** Modulus over {!Type_int}, {!Type_float}, {!Type_pos}, {!Type_color} and {!Type_point}*)
  | And  (** Boolean and*)
  | Or  (** Boolean or*)
  | Eq  (** Equality (polymorphic)*)
  | Ne  (** Inequality (polymorphic)*)
  | Lt  (** < (over {!Type_int}, {!Type_float} and {!Type_bool})*)
  | Gt  (** > (over {!Type_int}, {!Type_float} and {!Type_bool})*)
  | Le  (** <= (over {!Type_int}, {!Type_float} and {!Type_bool})*)
  | Ge  (** >= (over {!Type_int}, {!Type_float} and {!Type_bool})*)

(** Unary operator over the language*)
type unary_operator =
  | USub  (** Unary minus (over {!Type_int} and {!Type_float})*)
  | Not  (** Boolean negation*)
  | Head  (** Head of a list*)
  | Tail  (** Tail of a list*)
  | Floor  (** The biggest {!Type_int} smaller than the {!Type_float} given *)
  | Float_of_int
      (** The {!Type_float} corresponding to the {!Type_int} given *)
  | Cos  (** Cosinus function over {!Type_float}*)
  | Sin  (** Sinus function over {!Type_float}*)

(** Accessors to field of {!Type_point}, {!Type_pos} and {!Type_color}*)
type field_accessor =
  | Color_accessor
  | Position_accessor
  | X_accessor
  | Y_accessor
  | Blue_accessor
  | Red_accessor
  | Green_accessor

(** The expressions of the language. Only non-obvious cases are commented below. Every expression contains an {!Annotation.t}*)
type expression =
  | Constant_i of int * Annotation.t
  | Constant_f of float * Annotation.t
  | Constant_b of bool * Annotation.t
  | Pos of expression * expression * Annotation.t
      (** Creates a {!Type_pos} from the two expressions in argument.*)
  | Color of expression * expression * expression * Annotation.t
      (** Creates a {!Type_color} from the three expressions in argument.*)
  | Point of expression * expression * Annotation.t
      (** Creates a {!Type_point} from the two expressions in argument.*)
  | Variable of string * Annotation.t
  | Binary_operator of binary_operator * expression * expression * Annotation.t
  | Unary_operator of unary_operator * expression * Annotation.t
  | Field_accessor of field_accessor * expression * Annotation.t
  | List of expression list * Annotation.t
  | Cons of expression * expression * Annotation.t

(** The statements of the language. Only non-obvious cases are commented below. Every statement contains an {!Annotation.t}, except {!Nop}*)
type statement =
  | Assignment of expression * expression * Annotation.t
  | Variable_declaration of string * type_expr * Annotation.t
  | Block of statement list * Annotation.t
      (** Block of consecutive statements. Declarations in this block are local to it*)
  | IfThenElse of expression * statement * statement * Annotation.t
  | For of
      string * expression * expression * expression * statement * Annotation.t
      (** For loop. [For(str,init,target,step,body,annot)] starts by initialising variable [str] (which must be declared) to the value of [init], and executes [body] and then increments [str] by the value of [step] as long as the value in [str] is smaller than that of [target]. [target] is reevaluated at each step. Can be used for {!Type_int} or {!Type_float} variables*)
  | Foreach of string * expression * statement * Annotation.t
      (** [Foreach(str,list,body,annotation)] requires that [list] is a {!Type_list}. It applies [body] to every element of the list (from left to right). In [body], the current element of the loop is stored in variable [str] (which is not required to be declared)*)
  | Draw of expression * Annotation.t
      (** [Draw(point,annot)] draws [point] on the window of the program. [point] must be of type {!Type_point}*)
  | Nop
  | Print of expression * Annotation.t
      (** [Print(expr,annot)] displays the value of [expr] in the terminal (for debugging purposes)*)

(** Argument of the program*)
type argument = Argument of string * type_expr * Annotation.t

(** Type that represent a Stippled program. It contains a (possibly empty) list of declaration of arguments that will be read on the arguments of the interpreter, and a statement that is the body of the program. Every type can be read of argument, except of list of non-base type (i.e. list of int can be read, but not list of list of int).*)
type program = Program of argument list * statement

val string_of_type_expr : type_expr -> string
val string_of_binary_operator : binary_operator -> string
val string_of_unary_operator : unary_operator -> string
val string_of_field_accessor : field_accessor -> string
val string_of_expression : expression -> string
val string_of_pos : expression * expression -> string
val string_of_color : expression * expression * expression -> string
val string_of_list : expression list -> string
val pp_statement : Format.formatter -> statement -> unit
val pp_argument : Format.formatter -> argument -> unit
val pp_program : Format.formatter -> program -> unit
