(** Module that contain the interpreter of the Stippled language.*)

val interpret_prg : Ast.program -> string list -> unit
(** [interpret_prg program arguments] executes [program] over the arguments present in [arguments]. [arguments] are given as strings that represent the data.
    Integer, float and boolean are represented naturally, positions, colors and points as tuple (e.g. "((1,1),(0,0,0))" is a point), and list are represented with brackets (e.g. "\[1,2,3\]" is a list). Lists of lists are not recognized*)
