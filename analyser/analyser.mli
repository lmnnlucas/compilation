(**Module that is the entry point of the semantic analyser.*)

val analyser : Ast.program -> string -> Ast.program * bool
(** Entry point of the semantic analyser. Applies all analysis phases. [analyser program text] returns [modified_program,correct] where [modified_program] is the program after analysis (with simplifications performed, for example), and [correct] is [true] if and only if the program can be safely executed (i.e., no error has been detected). [text] is the string analysed to construct the program (to create the error report).
    If errors are detected, they are displayed by this function.*)
