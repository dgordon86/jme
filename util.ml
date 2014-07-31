let checkmatrix m = 
            let size = List.length (List.hd m) in
             List.iter (fun chksize -> 
                        if List.length chksize != size 
                        then raise (Failure ("unequal row sizes in matrix"))) m 
   
   
   (* print_endline ("\n[" ^ (String.concat ";\n " (List.map (fun lexpr -> "" ^ (String.concat "," (List.map (fun e -> Ast.string_of_expr e) lexpr ))  ^ "" ) m))
                ^ "]")*)