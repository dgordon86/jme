(* Data types of language *)
type dtypes =
    Null
    |Int of int
    |Float of float
    |Boolean of bool
    |String of string
    | Vector of dtypes list
    
    
let string_of_dtype = function
    Int(s) -> "Int " 
    | Float(s) -> "Float"
    | Boolean(s) -> "Boolean"
    | String(s) -> "String"
    | Vector(s) -> "Vector"
    | Null -> "Null"

let rec string_of_expr = function
    Int(s) -> string_of_int s
    | Float(s) -> string_of_float s
    | Boolean(s) -> string_of_bool s
    | String(s) -> s
    | Vector(s) -> "[" ^ String.concat "," (List.map string_of_expr s) ^ "]"
    | Null -> "null"
    
let add a b = 
    match a, b with
    Int(a), Int(b) -> Int (a + b)
    | Float(a), Float(b) -> Float (a +. b)
    | Int(a), Float(b) -> Float ((float_of_int a) +. b)
    | Float(a), Int(b) -> Float (a +. (float_of_int b))
    | String(a), String(b) -> String (a ^ b)
    |_ -> raise(Failure("invalid addition"))

let subtract a b = 
    match a, b with
    Int(a), Int(b) -> Int (a - b)
    | Float(a), Float(b) -> Float (a -. b)
    | Int(a), Float(b) -> Float ((float_of_int a) -. b)
    | Float(a), Int(b) -> Float (a -. (float_of_int b))
    |_ -> raise(Failure("invalid subtraction"))

let multiply a b =
    match a, b with
    Int(a), Int(b) -> Int (a * b)
    | Float(a), Float(b) -> Float (a *. b)
    | Int(a), Float(b) -> Float ((float_of_int a) *. b)
    | Float(a), Int(b) -> Float (a *. (float_of_int b))
    |_ -> raise(Failure("invalid multiplication"))

let divide a b =
    match a, b with
    Int(a), Int(b) -> Int (a / b)
    | Float(a), Float(b) -> Float (a /. b)
    | Int(a), Float(b) -> Float ((float_of_int a) /. b)
    | Float(a), Int(b) -> Float (a /. (float_of_int b))
    |_ -> raise(Failure("invalid addition"))

let equal a b =
    match a, b with
    Int(a), Int(b) -> Boolean(a = b)
    | Float(a), Float(b) -> Boolean (a = b)
    | Boolean(a), Boolean(b) -> Boolean (a = b)
    |_ -> Boolean(false)
    
let nequal a b =
    match a, b with
    Int(a), Int(b) -> Boolean(a != b)
    | Float(a), Float(b) -> Boolean (a != b)
    | Boolean(a), Boolean(b) -> Boolean (a != b)
    |_ -> Boolean(false)
    
let lessthn a b =
    match a, b with
    Int(a), Int(b) -> Boolean(a < b)
    | Float(a), Float(b) -> Boolean (a < b)
    |_ -> Boolean(false)
    
let lessthneq a b =
    match a, b with
    Int(a), Int(b) -> Boolean(a <= b)
    | Float(a), Float(b) -> Boolean (a <= b)
    |_ -> Boolean(false)

let greatthn a b =
    match a, b with
    Int(a), Int(b) -> Boolean(a > b)
    | Float(a), Float(b) -> Boolean (a > b)
    |_ -> Boolean(false)
    
let greatthneq a b =
    match a, b with
    Int(a), Int(b) -> Boolean(a >= b)
    | Float(a), Float(b) -> Boolean (a >= b)
    |_ -> Boolean(false)
    
let stripQuotes str =
    String.sub str 1 ((String.length str) -2)

let to_Bool = function
    Boolean i -> i
    |_ -> raise(Failure "invalid boolean")
    
let cast_int = function
    Int i  -> i
    | _     -> raise(Failure "invalid integer")