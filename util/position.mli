(** Module that represent positions in the analysed text. Useful for error reporting.*)

type t = Lexing.position * Lexing.position

val pp_position : Format.formatter -> string * t -> unit
(** Pretty-printer for a text and a position within it : (for argument [text,pos] displays the position and the substring of [text] delimited by [pos])*)
