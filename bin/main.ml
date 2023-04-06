open Language_parser

module Command_line = struct
  let parse_from_file = ref true
  let bypass_error = ref false

  let arg_spec_list =
    [
      ( "-from-stdin",
        Arg.Clear parse_from_file,
        " if present, the argument is a string to parse, otherwise it is the \
         name of the file containing the program." );
      ( "-bypass-error",
        Arg.Set bypass_error,
        " if present, executes incorrect programs -- use to your own risk." );
    ]

  let usage_msg =
    "Usage: " ^ Sys.argv.(0)
    ^ " [argument]\n\
       Parses the argument with the parser and displays the execution of the \
       parser step by step.\n\
       [argument] is either the string to parse or the filename to be analysed \
       (if [-from-stdin] is present)\n"

  let parse () =
    let res = ref None in
    let args = ref [] in
    Arg.parse (Arg.align arg_spec_list)
      (fun a ->
        match !res with None -> res := Some a | Some _ -> args := a :: !args)
      usage_msg;
    match !res with
    | None ->
        Arg.usage arg_spec_list usage_msg;
        exit 0
    | Some s -> (s, List.rev !args)
end

let input, args = Command_line.parse ()

let text, lexbuf =
  match !Command_line.parse_from_file with
  | true -> MenhirLib.LexerUtil.read input
  | false -> (input, Lexing.from_string input)

let program = Parser.main Lexer.token lexbuf
let () = Format.printf "@[<v 0>Raw_program:@,%a@,%!" Ast.pp_program program
let program, continue = Analyser.analyser program text
let () = Format.printf "Program after analysis:@,%a@,%!" Ast.pp_program program

let () =
  if continue || !Command_line.bypass_error then
    Interpreter.interpret_prg program args
  else Format.printf "Incorect program, stopping here@,"

let () = Format.printf "@]"
