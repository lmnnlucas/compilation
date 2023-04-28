open Util
open Ast

(*
    Réalisez ici l’analyse de type d’un programme. Faites une sous-fonction récursive pour les expressions et les statements.

    L’idée est la même que dans le langage du cours : pour chaque élément, commencez par typer ses sous-éléments. Ensuite, en fonction de l’élément et du type de ses sous-éléments, déterminez son type, et placez-le dans l’annotation qui l’accompagne.
    Seules les expressions peuvent ainsi être typées.

    Les fonctions devront manipuler un environnement qui associera chaque variable à son type. Les déclarations fixent ces types et ces types sont vérifiés lors de chaque utilisation d’une variable.

    Attention, les déclarations sont locales à un bloc, vous devez donc utiliser les fonctions Environment.add_layer et Environment.remove_layer pour le gérer.

    Cette fonction doit également effectuer les rapports d’erreur et d’avertissement dans [report].

    Ces fonction font un pattern matching sur leur argument principal et traitent chaque cas séparément. Elles font uniquement des effet de bord.
    Par exemple : type_expression : Ast.type_expr annotation -> Util.Error_report.t -> Ast.expression -> unit

    Vous pouvez également effectuer ici (en même temps ou dans une fonction séparée) une analyse d’initialisation des variables (auquel cas, il faut ajouter un argument supplémentaire à ce qui est décrit ci-dessus).

    Vous préciserez ce que vous avez traité dans votre rapport.
*)

  let is_operation_binop = function
    | Add | Sub | Mul | Div | Mod -> true
    | _ -> false
  
  let is_bool_only_binop = function
    | And | Or -> true
    | _ -> false

  let is_comparison_binop = function
  | Eq | Ne | Lt | Gt | Le | Ge -> true
  | _ -> false 
   
  let rec type_expression report env expression =

    match expression with
    | Constant_i(_,a) -> Annotation.set_type a Type_int
    | Constant_f(_,a) -> Annotation.set_type a Type_float
    | Constant_b(_,a) -> Annotation.set_type a Type_bool
    | Variable(name,a) -> (
      match Environment.get env name with
      | Some t -> Annotation.set_type a t
      | None -> Error_report.add_error report ("No type found for the desired variable", Annotation.get_pos a); a
    )
    | Unary_operator(unop,e,a) -> (
      let type_e = Annotation.get_type (type_expression report env e) in
      match unop, type_e with
      | USub, Some (Type_int|Type_float) -> a
      | USub, _ -> Error_report.add_error report ("Minus operator only accept Int or Float type", Annotation.get_pos a);a
      | Not, Some Type_bool -> a
      | Not, _ -> Error_report.add_error report ("Not operator only accept Bool type",Annotation.get_pos a); a
      | Head, Some Type_list(_) -> a
      | Head, _ -> Error_report.add_error report ("Head operator only accept List type", Annotation.get_pos a); a
      | Tail, Some Type_list(_) -> a
      | Tail, _ -> Error_report.add_error report ("Tail operator only accept List type", Annotation.get_pos a); a
      | Floor, Some Type_float -> a
      | Floor, _ -> Error_report.add_error report ("Floor operator only accept Float type", Annotation.get_pos a); a
      | Float_of_int, Some Type_float -> a
      | Float_of_int, _ -> Error_report.add_error report ("Float_of_int operator only accept Float type", Annotation.get_pos a); a
      | Cos, Some Type_float -> a
      | Cos, _ -> Error_report.add_error report ("Cos operator only accept Float type", Annotation.get_pos a); a
      | Sin, Some Type_float -> a
      | Sin, _ -> Error_report.add_error report ("Sin operator only accept Float type", Annotation.get_pos a); a 
    )
    | Binary_operator(binop,e1,e2,a) -> Error_report.add_error report ("test", Annotation.get_pos a); a
    | _ -> ()

  let rec type_statement report env statement =
    match statement with
    | Assignment(e1,e2,a) -> (
      let a = type_expression report env e1 in
      Error_report.add_warning report ("Test",Annotation.get_pos a)
    )
    | _ -> ()
  
  let type_analyser report program =
    let type_environment = Environment.new_environment () in
      Environment.add type_environment "x" Type_int;
      match program with 
      | (al,s) -> type_statement report type_environment s