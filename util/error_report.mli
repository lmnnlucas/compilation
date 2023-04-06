(** Module to reports errors and warnings, and prints them*)

type error = string * Position.t
(** An error is a message and a position in the program.*)

type t
(** The type of error report*)

val create_empty : string -> t
(** Creates an empty error report. The string argument is the text parsed, in which positions will refer to part of it.*)

val add_error : t -> error -> unit
(** [add_error report error] adds [error] as an error to the error report.*)

val add_warning : t -> error -> unit
(** [add_warning report warning] adds [warning] as a warning to the error report.*)

val has_errors : t -> bool
(** [has_errors report] returns [true] if [report] contains at least one error, and [false] otherwise.*)

val pp_errors : Format.formatter -> t -> unit
(** Pretty printer for an error report.*)