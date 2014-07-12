(* Data types of language *)
type dtypes =
    Null
    |Int of int
    |Float of float
    
    
let string_of_dtype = function
    Int(s) -> "Int " 
    | Float(s) -> "Float"
    | Null -> "Null"

let string_of_expr = function
    Int(s) -> string_of_int s
    | Float(s) -> string_of_float s
    | Null -> "null"
    
let add a b = 
    match a, b with
    Int(a), Int(b) -> Int (a + b)
    | Float(a), Float(b) -> Float (a +. b)

let subtract a b = 
    match a, b with
    Int(a), Int(b) -> Int (a - b)
    | Float(a), Float(b) -> Float (a -. b)

let multiply a b =
    match a, b with
    Int(a), Int(b) -> Int (a * b)
    | Float(a), Float(b) -> Float (a *. b)

let divide a b =
    match a, b with
    Int(a), Int(b) -> Int (a / b)
    | Float(a), Float(b) -> Float (a /. b)
    
let cast_int = function
    Int i  -> i
    | _     -> raise(Failure "invalid primitive")