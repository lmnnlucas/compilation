(** Module that represent environments that are useful for both interpreter and analyser.
    An environment is a stack of maps from names (string) to some value.
    The stack is there to handle scopes in a program.*)

type 'a t
(** The environment type*)

val new_environment : unit -> 'a t
(** Creates an empty environment.*)

val add_layer : 'a t -> unit
(** Adds a new (empty) layer in the stack.*)

val remove_layer : 'a t -> unit
(** Remove the topmost layer of the stack, forgetting all values stored there.*)

val add : 'a t -> string -> 'a -> unit
(** [add environement name value] associates [value] to [name] in the topmost layer, even if name is already associated below. Useful for handling scopes.*)

val add_ref : 'a t -> string -> 'a ref -> unit
(** [add_ref environment name ref] associates the reference [ref] to [name] in the topmost layer, even if name is already associated below. Only useful for the interpretation of the Foreach construction.*)

val get : 'a t -> string -> 'a option
(** [get environment name] gets [Some v] if v is the value associated to [name] in the topmost layer where it is defined, and returns [None] if it is not defined.*)

val modify : 'a t -> string -> 'a -> unit
(** [modify environment name value] replaces the topmost value associated to [name] by [value] if it is defined, and otherwise makes that association in the topmost level.*)

val is_def_in_current_layer : 'a t -> string -> bool
(** [is_def_in_current_layer env name] returns true if and only if [name] is set in the top layer of the environment, and false otherwise (even if it is set below). Useful for detecting duplicate declarations in a bloc.*)
