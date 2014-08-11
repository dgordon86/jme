
type action = Ast | Interpret | Bytecode | Compile
  
let _ =
  let action = if Array.length Sys.argv > 1 then
    List.assoc Sys.argv.(1) [ ("-a", Ast);
			      ("-i", Interpret);
			      ("-b", Bytecode);
			      ("-c", Compile)]
  else Compile in
  
  (*let stdlib = input_all (open_in "stdlib.jme") in
  let currprog = input_all stdin in 
  let fullprog = Lexing.from_string (stdlib ^ currprog) in *)
  let program = if Array.length Sys.argv = 3 && Sys.argv.(2) = "-wo" then (Parser.program Scanner.token (Lexing.from_channel stdin)) 
                else (Parser.program Scanner.token (Lexing.from_string ((Util.input_all (open_in "stdlib.jme")) ^ (Util.input_all stdin)))) in
  match action with
    Ast -> let listing = Ast.string_of_program program
           in print_string listing
  | Interpret -> ignore (Interpret.run program)
  | Bytecode -> let listing =
      Bytecode.string_of_prog (Compile.translate program)
    in print_endline listing
  | Compile -> Execute.execute_prog (Compile.translate program)
  
  
  
 
