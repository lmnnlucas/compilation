module Command_line = struct
  let parse_from_file = ref true

  let arg_spec_list =
    [
      ( "-from-stdin",
        Arg.Clear parse_from_file,
        " if present, the argument is a string to parse, otherwise it is the \
         name of the file containing the program." );
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
    Arg.parse (Arg.align arg_spec_list)
      (fun a ->
        match !res with
        | None -> res := Some a
        | Some _ -> raise (Arg.Bad "Got too many inputs"))
      usage_msg;
    match !res with
    | None ->
        Arg.usage arg_spec_list usage_msg;
        exit 0
    | Some s -> s
end

let str = Command_line.parse ()
let () = Format.printf "Coucou\n%!"

module Language_Sign :
  Parser_visualiser.Parsing.parser_decorated
    with type value_parsed = Ast.program = struct
  type value_parsed = Ast.program

  let parser_name = "Parser"
  let parser_path = "parser"
  let error_strategy = Parser_visualiser.Parsing.Stop

  module Lexer = Language_parser.Lexer
  module Parser = Language_parser.Parser
end

module ProgParser =
  Parser_visualiser.Parsing.MakeWithDefaultMessage (Language_Sign)

let parse_string str = ProgParser.parse_string_interact str true None None
let parse_file str = ProgParser.parse_file_interact str true None None

let main =
  if !Command_line.parse_from_file then parse_file str else parse_string str

let () =
  match main with
  | Some prg ->
      Format.printf "@[<v 0>Program parsed:@,%a@,@]" Ast.pp_program prg
  | None -> Format.printf "No program obtained\n"