type t = Lexing.position * Lexing.position

let show text (positions : t) =
  MenhirLib.ErrorReports.extract text positions
  |> MenhirLib.ErrorReports.sanitize |> MenhirLib.ErrorReports.compress
  |> MenhirLib.ErrorReports.shorten 20

let pp_position fmt (text, pos) =
  Format.fprintf fmt "%s@,%s"
    (List.hd (String.split_on_char '\n' (MenhirLib.LexerUtil.range pos)))
    (show text pos)