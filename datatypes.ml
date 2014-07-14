(* Data types of language *)
type dtypes =
    Null
    |Int of int
    |Float of float
    |Boolean of bool
    
    
let string_of_dtype = function
    Int(s) -> "Int " 
    | Float(s) -> "Float"
    | Boolean(s) -> "Boolean"
    | Null -> "Null"

let string_of_expr = function
    Int(s) -> string_of_int s
    | Float(s) -> string_of_float s
    | Boolean(s) -> string_of_bool s
    | Null -> "null"
    
let add a b = 
    match a, b with
    Int(a), Int(b) -> Int (a + b)
    | Float(a), Float(b) -> Float (a +. b)
    | Int(a), Float(b) -> Float ((float_of_int a) +. b)
    | Float(a), Int(b) -> Float (a +. (float_of_int b))
    |_ -> raise(Failure("invalid addition"))

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

let equal a b =
    match a, b with
    Int(a), Int(b) -> Boolean(a = b)
    | Float(a), Float(b) -> Boolean (a = b)
    | Boolean(a), Boolean(b) -> Boolean (a = b)
    |_ -> Boolean(false)
    
    
let cast_int = function
    Int i  -> i
    | _     -> raise(Failure "invalid primitive")