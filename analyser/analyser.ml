open Util

(*
   Aide :

   Ce fichier est le point d’entrée de l’analyseur sémantique. La version initiale de l’analyseur appelle l’analyseur de type, puis le simplificateur, et affiche ensuite les erreurs éventuellement ajoutées lors de ces fonctions.

   Cette version devrait être suffisante pour la partie principale du compilateur. Vous pouvez cependant être amené à ajouter d’autres analyses (par exemple, pour l’extension sur les casts implicites). Dans ce cas, créez les fonctions d’analyses pertinentes (de préférence dans d’autres fichiers), puis appelez-les simplement depuis cette fonction.

   Vous pouvez simplement ajouter un autre fichier dans ce dossier. Si vous créez le fichier "toto.ml", les fonctions qui y sont définies en préfixant leur nom par Toto (par exemple, si "toto.ml" contient la fonction "foo", alors vous pouvez appeler "Toto.foo" ici).

   Aucun fichier de configuration n’est à modifier pour cela.

*)

let analyser program text =
  let report = Error_report.create_empty text in
  Type_analyser.type_analyser report program;
  let program = Simplifier.simplifier program in
  Format.printf "@[<v 0>%a@]@," Error_report.pp_errors report;
  let incorrect = not (Error_report.has_errors report) in
  (program, incorrect)
