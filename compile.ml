open Ast
open Bytecode

module StringMap = Map.Make(String)

(* Symbol table: Information about all the names in scope *)
type env = {
    function_index : int StringMap.t; (* Index for each function *)
    global_index   : int StringMap.t; (* "Address" for global variables *)
    mutable local_index    : int StringMap.t; (* FP offset for args, locals *)
  }

(* val enum : int -> 'a list -> (int * 'a) list *)
let rec enum stride n = function
    [] -> []
  | hd::tl -> (n, hd) :: enum stride (n+stride) tl

(* val string_map_pairs StringMap 'a -> (int * 'a) list -> StringMap 'a  *)
let string_map_pairs map pairs =
  List.fold_left (fun m (i, n) -> StringMap.add n i m) map pairs
  
(*debug to view local variables *)
let print_locals s i =
    print_string (s ^ "->"); print_int i; print_endline "";;

(** Translate a program in AST form into a bytecode program.  Throw an
    exception if something is wrong, e.g., a reference to an unknown
    variable or function *)
let translate (globals, functions) =

  (* Allocate "addresses" for each global variable *)
  let global_indexes = string_map_pairs StringMap.empty (enum 1 0 globals) in

  (* Assign indexes to function names; built-in "print" is special *)
  let built_in_functions = StringMap.add "print" (-1) StringMap.empty in
  let built_in_functions = StringMap.add "width" (-2) built_in_functions in
  let built_in_functions = StringMap.add "height" (-3) built_in_functions in
  let built_in_functions = StringMap.add "sqrt" (-4) built_in_functions in
  let built_in_functions = StringMap.add "sort" (-5) built_in_functions in
  let built_in_functions = StringMap.add "mhas" (-6) built_in_functions in
  let built_in_functions = StringMap.add "is_Int" (-7) built_in_functions in
  let built_in_functions = StringMap.add "is_Float" (-8) built_in_functions in
  let built_in_functions = StringMap.add "is_Bool" (-9) built_in_functions in
  let built_in_functions = StringMap.add "is_String" (-10) built_in_functions in
  let built_in_functions = StringMap.add "is_Vector" (-11) built_in_functions in
  let built_in_functions = StringMap.add "is_Matrix" (-12) built_in_functions in
  let built_in_functions = StringMap.add "is_Map" (-13) built_in_functions in
  let function_indexes = string_map_pairs built_in_functions
      (enum 1 1 (List.map (fun f -> f.fname) functions)) in

  (* Translate a function in AST form into a list of bytecode statements *)
  let translate env fdecl =
    (* Bookkeeping: FP offsets for locals and arguments *)
    let num_formals = List.length fdecl.formals
    and local_offsets = enum 1 1 fdecl.locals
    and formal_offsets = enum (-1) (-2) fdecl.formals in
    let env = { env with local_index = string_map_pairs
		  StringMap.empty (local_offsets @ formal_offsets) } in

    let rec expr = function
	Literal i -> [Lit (Datatypes.Int i)]
      | Float f -> [Lit (Datatypes.Float f)]
      | Boolean b -> [Lit (Datatypes.Boolean b)]
      | String s -> [Lit (Datatypes.String (Datatypes.stripQuotes s))]
      | Id s -> (*print_string "numformals: "; print_int num_formals; print_endline "";
                print_int (StringMap.cardinal env.local_index); print_endline ""; StringMap.iter print_locals env.local_index; *)
	  (try [Lfp (StringMap.find s env.local_index)]
          with Not_found -> try [Lod (StringMap.find s env.global_index)]
          with Not_found -> raise (Failure ("undeclared variable " ^ s)))
      | Binop (e1, op, e2) -> expr e1 @ expr e2 @ [Bin op]
      | Assign (s, e) -> expr e @
	  (try [Sfp (StringMap.find s env.local_index)]
  	  with Not_found -> try [Str (StringMap.find s env.global_index)]
	  with Not_found -> let lclindex = StringMap.cardinal env.local_index - num_formals + 1 in 
                        ignore(env.local_index <- StringMap.add s lclindex env.local_index); 
                        [Sfp lclindex]
            (*raise (Failure ("undeclared variable " ^ s))*))
      | VectRef (s, e) -> expr e @
	       (try [Lfpv (StringMap.find s env.local_index)]
  	         with Not_found -> try [Lodv (StringMap.find s env.global_index)]
	         with Not_found -> raise (Failure ("undeclared variable " ^ s)))
      | VectAssign (s, e1, e2) -> expr e2 @ expr e1 @
	       (try [Ulvec (StringMap.find s env.local_index)]
  	         with Not_found -> try [Ugvec (StringMap.find s env.global_index)]
	         with Not_found -> raise (Failure ("undeclared variable " ^ s)))
      | Call (fname, actuals) -> (try
	  (List.concat (List.map expr (List.rev actuals))) @
	  [Jsr (StringMap.find fname env.function_index) ]   
        with Not_found -> raise (Failure ("undefined function " ^ fname)))
      | Matrix m -> if List.length m = 1 then let vect = List.hd m in 
                        (List.concat (List.map expr vect)) @ [Vec (List.length vect) ] 
                    else 
                      let dimx = List.length m in
                      let dimy = List.length (List.hd m) in
                      ignore(Util.checkmatrix m); (List.concat (List.map expr (List.concat m))) @ [Mat (dimx,dimy) ] 
      | JMap m -> List.concat (List.map (fun (k,v) -> (expr v) @ (expr k) ) m) @ [Map ( List.length m) ]
      | VectorInit e -> expr e @ [Veci]
      | MatrixInit (x,y) -> expr x @ expr y @ [Mati]
      | MatxRef (s, x, y) -> expr x @ expr y @
            (try [Lfpm (StringMap.find s env.local_index)]
  	         with Not_found -> try [Lodm (StringMap.find s env.global_index)]
	         with Not_found -> raise (Failure ("undeclared variable " ^ s)))
      | MatxAssign (s, e1, e2, e3) -> expr e3 @ expr e1 @ expr e2 @
	       (try [Ulmat (StringMap.find s env.local_index)]
  	         with Not_found -> try [Ugmat (StringMap.find s env.global_index)]
	         with Not_found -> raise (Failure ("undeclared variable " ^ s)))
      | Noexpr -> []

    in let rec stmt = function
	Block sl     ->  List.concat (List.map stmt sl)
      | Expr e       -> expr e @ [Drp]
      | Return e     -> expr e @ [Rts num_formals]
      | If (p, t, f) -> let t' = stmt t and f' = stmt f in
	expr p @ [Beq(2 + List.length t')] @
	t' @ [Bra(1 + List.length f')] @ f'
      | For (e1, e2, e3, b) ->
	  stmt (Block([Expr(e1); While(e2, Block([b; Expr(e3)]))]))
      | While (e, b) ->
	  let b' = stmt b and e' = expr e in
	  [Bra (1+ List.length b')] @ b' @ e' @
	  [Bne (-(List.length b' + List.length e'))]

    in [Ent (StringMap.cardinal env.local_index - num_formals)] @      (* Entry: allocate space for locals *)
    stmt (Block fdecl.body) @  (* Body *)
    [Lit (Datatypes.Int 0); Rts num_formals]   (* Default = return 0 *)

  in let env = { function_index = function_indexes;
		 global_index = global_indexes;
		 local_index = StringMap.empty } in

  (* Code executed to start the program: Jsr main; halt *)
  let entry_function = try
    [Jsr (StringMap.find "main" function_indexes); Hlt]
  with Not_found -> raise (Failure ("no \"main\" function"))
  in
    
  (* Compile the functions *)
  let func_bodies = entry_function :: List.map (translate env) functions in

  (* Calculate function entry points by adding their lengths *)
  let (fun_offset_list, _) = List.fold_left
      (fun (l,i) f -> (i :: l, (i + List.length f))) ([],0) func_bodies in
  let func_offset = Array.of_list (List.rev fun_offset_list) in

  { num_globals = List.length globals;
    (* Concatenate the compiled functions and replace the function
       indexes in Jsr statements with PC values *)
    text = Array.of_list (List.map (function
	Jsr i when i > 0 -> Jsr func_offset.(i)
      | _ as s -> s) (List.concat func_bodies))
  }
