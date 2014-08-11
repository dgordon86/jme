open Datatypes


let checkmatrix m = 
            let size = List.length (List.hd m) in
             List.iter (fun chksize -> 
                        if List.length chksize != size 
                        then raise (Failure ("unequal row sizes in matrix"))) m 
   
   
let getMatxElement x y m =
    match x,y, m with
    Int(x), Int(y), Matrix(m) -> m.(x).(y)
    |_ -> raise(Failure("invalid matrix access"))
    

let getWidth s = 
    match s with
    | Vector(s) -> Int (Array.length (s))
    | Matrix(s) -> Int (Array.length s.(0))
    |_ -> raise(Failure(("width() cannot be applied to type " ^ Datatypes.string_of_dtype s ^ " " ^ Datatypes.string_of_expr s)))
    
let getHeight s = 
    match s with
    | Vector(s) -> Int (1)
    | Matrix(s) -> Int (Array.length s)
    |_ -> raise(Failure(("width() cannot be applied to type " ^ Datatypes.string_of_dtype s ^ " " ^ Datatypes.string_of_expr s)))
    
(*let updateEntry index data nvalue = 
    match index, data with
    Int(index), Vector(data) -> data.(index) <- nvalue
    | String(index), JMap(data) ->
        StringMap.add index nvalue data
    |_ -> raise(Failure("unexpected update of types"))*)