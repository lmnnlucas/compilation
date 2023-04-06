type error = string * Position.t

type t = {
  mutable errors : error list;
  mutable warnings : error list;
  text : string;
}

let create_empty text = { errors = []; warnings = []; text }
let add_error report error = report.errors <- error :: report.errors
let add_warning report warning = report.warnings <- warning :: report.warnings
let has_errors report = List.length report.errors > 0

let pp_errors fmt report =
  Format.fprintf fmt "@[<v 2>Errors :@,";
  List.iter
    (fun (e, pos) ->
      Format.fprintf fmt "@[<v 1>%a@,%s@]@," Position.pp_position
        (report.text, pos) e)
    (List.rev report.errors);
  Format.fprintf fmt "@]@,@[<v 2>Warnings :@,";
  List.iter
    (fun (e, pos) ->
      Format.fprintf fmt "@[<v 1>%a@,%s@]@," Position.pp_position
        (report.text, pos) e)
    (List.rev report.warnings);
  Format.fprintf fmt "@]@,"
