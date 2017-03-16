let debug = Common.debug "Main"
let fulldebug = Common.fulldebug "Main"
              
(* File reading and parsing *)

let rec parse_until_end chan accum = 
  try 
    let s = input_char chan in
    if s = '\r' then assert false 
    else
      (parse_until_end chan (accum^(String.make 1 s)))
  with End_of_file -> accum

let parse_string str =
      debug "Compilation starting" ;
      let lexbuf = Lexing.from_string str in
      debug "Lexer built" ;
      try 
           ScribbleParser.scribblefile ScribbleLexer.token lexbuf
         with
             ScribbleSyntax.Syntax_error (s,i) ->
	       (prerr_string ("Syntax error: "^s^" "^(Common.info_to_string i)^"\n");
                exit 1)
           | ScribbleSyntax.Parse_error (s,i)  ->
	     (prerr_string ("Parsing error: "^s^" "^(Common.info_to_string i)^"\n"); 
              exit 2)
                    
(* Parse the files entered as arguments *)
let parse_file file =
      let chan = open_in file in
      let raw_scribble = parse_until_end chan "" in
      fulldebug ("File "^file^" reads: \n"^raw_scribble) ;
      let sessionast = parse_string raw_scribble in
      let () = close_in chan in
      let () = debug ("Protocols from file "^file^" parsed:\n"^(Prettyprint.print_ast sessionast))
      in
      sessionast

let project protocol_body role =
      let g = Conversation.global_conversion protocol_body in
      let () = fulldebug ("Global type:\n"^(Prettyprint.print_globaltype g))
      in
      let wf = Wellformedness.check g in
      let () = debug ("Wellformed: "^(string_of_bool wf))
      in
      let tr = Projection.project role g in
      let () = fulldebug ("Raw local type:\n"^(Prettyprint.print_localtype tr)) in
      let tc = Projection.clean_local_role role tr in
      let () = fulldebug ("Cleaned Local type:") in
      let t  = Conversation.localnodetoAST tc in
      t
