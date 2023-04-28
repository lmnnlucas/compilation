open Ast

(* Codez ici le simplificateur de termes.

    Tout comme pour le langage du cours, l’idée consiste à remplacer les termes constants par le résultat de leur calcul.

    Faites une sous-fonctions récursive pour les expressions et les statements.
    Ces fonction font un pattern matching sur leur argument et traitent chaque cas séparément. Elles renvoient un argument de même type que celui reçu.
    Par exemple : simplify_expression : Ast.expression -> Ast.expression

    Les cas minimaux attendus sont les cas sur les entiers, les flottants, les booléens, ainsi que les if dont le test est constant, et les for qui ne s’exécutent jamais.

    Deux points qui peuvent vous permettre d’aller plus loin :
      - les expressions ne peuvent pas faire d’effet de bord ici, ce qui permet de simplifier des expressions pas nécessairement constantes.
      - Les types composés (point, position et couleur) peuvent également être simplifiés (e.g., (1,2) + (2,x) peut être simplifié en (3,2+x)).

    Vous détaillerez dans le rapport les différents cas que vous simplifiez dans votre simplificateur.
*)

let simplifier program = program

let rec simplify_expr expression = 
  let simplified_expr = 
    match expression with
    | Pos(e1,e2,annotation) -> 
      let simp_e1 = simplify_expr e1 in
      let simp_e2 = simplify_expr e2 in
      Pos(simp_e1,simp_e2,annotation)
    | Color(e1,e2,e3,annotation) -> 
      let simp_e1 = simplify_expr e1 in
      let simp_e2 = simplify_expr e2 in
      let simp_e3 = simplify_expr e3 in
      Color(simp_e1,simp_e2,simp_e3,annotation)
    | Point(e1,e2,annotation) ->
      let simp_e1 = simplify_expr e1 in
      let simp_e2 = simplify_expr e2 in
      Point(simp_e1,simp_e2,annotation)
    | Unary_operator(op,e,annotation) -> (
      let simp_e = simplify_expr e in
      match op, simp_e with
      | USub, Constant_i(v,a) -> Constant_i((-v),a)
      | USub, Constant_f(v,a) -> Constant_f((-.v),a) 
      | Not, Constant_b(v,a) -> Constant_b((not v), a)
      | Floor, Constant_f(v,a) -> Constant_f((floor v),a)
      | Float_of_int, Constant_i(v,a) -> Constant_f((float_of_int v),a)
      | Cos, Constant_f(v,a) -> Constant_f((cos v),a)
      | Sin, Constant_f(v,a) -> Constant_f((sin v),a)
      | _ -> Unary_operator(op,simp_e,annotation) 
    ) 
    | Binary_operator(op,e1,e2,annotation) -> (
      let simp_e1 = simplify_expr e1 in
      let simp_e2 = simplify_expr e2 in
      match (op, simp_e1, simp_e2) with
      | Add, Constant_i (v1,a1), Constant_i (v2,_) -> Constant_i ((v1+v2),a1)
      | Sub, Constant_i (v1,a1), Constant_i (v2,_) -> Constant_i ((v1-v2),a1)
      | Mul, Constant_i (v1,a1), Constant_i (v2,_) -> Constant_i ((v1*v2),a1)
      | Div, Constant_i (v1,a1), Constant_i (v2,_) -> Constant_i ((v1/v2),a1)
      | Mod, Constant_i (v1,a1), Constant_i (v2,_) -> Constant_i ((v1 mod v2),a1)
      | Add, Constant_f (v1,a1), Constant_f (v2,_) -> Constant_f ((v1+.v2),a1)
      | Sub, Constant_f (v1,a1), Constant_f (v2,_) -> Constant_f ((v1-.v2),a1)
      | Mul, Constant_f (v1,a1), Constant_f (v2,_) -> Constant_f ((v1*.v2),a1)
      | Div, Constant_f (v1,a1), Constant_f (v2,_) -> Constant_f ((v1/.v2),a1)
      | Mod, Constant_f (v1,a1), Constant_f (v2,_) -> Constant_f ((mod_float v1 v2),a1)
      | And, Constant_b (v1,a1), Constant_b (v2,_) -> Constant_b ((v1 && v2),a1)
      | Or, Constant_b (v1,a1), Constant_b (v2,_) -> Constant_b ((v1 || v2),a1)
      | Eq, Constant_b (v1,a1), Constant_b (v2,_) -> Constant_b ((v1 == v2),a1)
      | Eq, Constant_i (v1,a1), Constant_i (v2,_) -> Constant_b ((v1 == v2), a1)
      | Eq, Constant_f (v1,a1), Constant_f (v2,_) -> Constant_b ((v1 == v2),a1)
      | Ne, Constant_b (v1,a1), Constant_b (v2,_) -> Constant_b ((v1 != v2),a1)
      | Ne, Constant_i (v1,a1), Constant_i (v2,_) -> Constant_b ((v1 != v2), a1)
      | Ne, Constant_f (v1,a1), Constant_f (v2,_) -> Constant_b ((v1 != v2),a1)
      | Le, Constant_i (v1,a1), Constant_i (v2,_) -> Constant_b ((v1 <= v2), a1)
      | Le, Constant_f (v1,a1), Constant_f (v2,_) -> Constant_b ((v1 <= v2), a1)
      | Ge, Constant_i (v1,a1), Constant_i (v2,_) -> Constant_b ((v1 >= v2), a1)
      | Ge, Constant_f (v1,a1), Constant_f (v2,_) -> Constant_b ((v1 >= v2), a1)
      | Lt, Constant_i (v1,a1), Constant_i (v2,_) -> Constant_b ((v1 < v2), a1)
      | Lt, Constant_f (v1,a1), Constant_f (v2,_) -> Constant_b ((v1 < v2), a1)
      | Gt, Constant_i (v1,a1), Constant_i (v2,_) -> Constant_b ((v1 > v2), a1)
      | Gt, Constant_f (v1,a1), Constant_f (v2,_) -> Constant_b ((v1 > v2), a1)
      | _ -> Binary_operator(op,simp_e1,simp_e2,annotation)
    )
    | Field_accessor(fa,e,a) -> (
      let simp_e = simplify_expr e in
      Field_accessor(fa,simp_e,a)
    )
    | List(elist, a) -> (
      let simp_elist = (List.map (fun e -> (simplify_expr e)) elist) in
      List(simp_elist, a)
    )
    | Cons(e1,e2,a) -> (
      let simp_e1 = simplify_expr e1 in
      let simp_e2 = simplify_expr e2 in
      Cons(simp_e1,simp_e2,a)
    )
    | _ -> expression
  in
  simplified_expr

let rec simplify_statement statement = 
  let simplified_statement = 
    match statement with
    | Assignment(e1,e2,a) -> (
      let simp_e1 = simplify_expr e1 in
      let simp_e2 = simplify_expr e2 in
      Assignment(simp_e1,simp_e2,a)
    )
    | Block(slist, a) -> (
      let simp_slist = (List.map (fun s -> (simplify_statement s)) slist) in
      Block(simp_slist,a)
    )
    | IfThenElse(cond,s1,s2,a) -> (
      let simp_s1 = simplify_statement s1 in
      let simp_s2 = simplify_statement s2 in
      let simp_cond = simplify_expr cond in

      match simp_cond with
      | Constant_b(true,_) -> simp_s1
      | Constant_b(false,_) -> simp_s2
      | _ -> IfThenElse(simp_cond,simp_s1,simp_s2,a)
    )
    | For(var_name,init,target,step,body,a) -> (
      let simp_init = simplify_expr init in
      let simp_target = simplify_expr target in
      let simp_step = simplify_expr step in
      let simp_body = simplify_statement body in

      match simp_init, simp_target, simp_step with
      | Constant_i(vi,_), Constant_i(vt,_), Constant_i(vs,_) -> if (vi < vt && vs > 0) || (vi > vt  && vs < 0) then 
                                                                  For(var_name,simp_init,simp_target,simp_step,simp_body,a)
                                                                else 
                                                                  Nop;
      | Constant_f(vi,_), Constant_f(vt,_), Constant_f(vs,_) -> if (vi < vt && vs > 0.) || (vi > vt  && vs < 0.) then 
                                                                  For(var_name,simp_init,simp_target,simp_step,simp_body,a)
                                                                else 
                                                                  Nop;
      | _ -> For(var_name,simp_init,simp_target,simp_step,simp_body,a)
    ) 
    | Foreach(var,list,body,a) -> (
      let simp_body = simplify_statement body in
      Foreach(var,list,simp_body,a)
    )
    | Draw(e,a) -> (
      let simp_e = simplify_expr e in
      Draw(simp_e,a)
    )
    | Nop -> Nop
    | Print(e,a) -> (
      let simp_e = simplify_expr e in
      Print(simp_e,a)
    )
    | _ -> statement 
  in
  simplified_statement