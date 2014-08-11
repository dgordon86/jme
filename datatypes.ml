(* Data types of language *)
module StringMap = Map.Make(String)

type dtypes =
      Null
    | Int of int
    | Float of float
    | Boolean of bool
    | String of string
    | Vector of dtypes array
    | Matrix of dtypes array array
    | JMap of dtypes StringMap.t
    
    
let string_of_dtype = function
    Int(s) -> "Int " 
    | Float(s) -> "Float"
    | Boolean(s) -> "Boolean"
    | String(s) -> "String"
    | Vector(s) -> "Vector"
    | Matrix(s) -> "Matrix"
    | JMap(s) -> "Map"
    | Null -> "Null"

let rec string_of_expr = function
    Int(s) -> string_of_int s
    | Float(s) -> string_of_float s
    | Boolean(s) -> string_of_bool s
    | String(s) -> s
    | Vector(s) -> "[" ^ String.concat "," (List.map string_of_expr (Array.to_list s)) ^ "]"
    | Matrix(s) ->  "[" ^ (String.concat ";\n " (List.map (fun lexpr -> "" ^ 
                        (String.concat "," (List.map (fun e -> string_of_expr e) (Array.to_list lexpr) ))  ^ "" ) (Array.to_list s)))
                ^ "]"
    | JMap(s) -> let string_pairs k v str= (k ^ "=>" ^ 
                        string_of_expr v ^ ", " ^ str) in
                "|" ^ (StringMap.fold string_pairs s "") ^ "|"
    | Null -> "null"

let getVectElement i v =
    match i, v with
    Int(i), Vector(v) -> v.(i)
    | String(s), JMap(m) -> StringMap.find s m
    |_ -> raise(Failure("invalid vector access"))

let updateVectElement i v nv =
    match i, v with
    Int(i), Vector(v) -> v.(i) <- nv
    |_ -> raise(Failure("Invalid update structure"))
    
let printstack stack =
    for i = 0 to (Array.length stack -1) do
        print_endline ((string_of_dtype stack.(i)) ^ " " ^ (string_of_expr stack.(i)))
    done
    
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
    Int(a), Int(b) -> (* if division by integer yields a remainder, implicitly convert to float *)
                      if a mod b > 0 then 
                      Float((float_of_int a) /. (float_of_int b)) else Int (a / b)
    | Float(a), Float(b) -> Float (a /. b)
    | Int(a), Float(b) -> Float ((float_of_int a) /. b)
    | Float(a), Int(b) -> Float (a /. (float_of_int b))
    |_ -> raise(Failure("invalid division"))

let power a b =
    match a, b with
    Int(a), Int(b) -> (* if exponentiation by integer implicitly convert to float *)
                      Float((float_of_int a) ** (float_of_int b))
    | Float(a), Float(b) -> Float (a ** b)
    | Int(a), Float(b) -> Float ((float_of_int a) ** b)
    | Float(a), Int(b) -> Float (a ** (float_of_int b))
    |_ -> raise(Failure("invalid arguments for exponents"))
    
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

let to_Float i = 
    match i with
    Int i -> float_of_int i
    | Float i -> i
    |_ -> raise(Failure ("Expected Float or Integer got " ^ string_of_dtype i ^ " " ^ string_of_expr i))

let to_Bool i = 
    match i with
    Boolean i -> i
    |_ -> raise(Failure ("Expected Boolean got " ^ string_of_dtype i ^ " " ^ string_of_expr i))

let to_Vector i = 
    match i with
    Vector i -> i
    |_ -> raise(Failure ("Expected Vector got " ^ string_of_dtype i ^ " " ^ string_of_expr i))

let to_JMap m =
    match m with
    JMap m -> m
     |_ -> raise(Failure ("Expected Map got " ^ string_of_dtype m ^ " " ^ string_of_expr m))
    
let to_Matrix i = 
    match i with
    Matrix i -> i
    |_ -> raise(Failure ("Expected Matrix got " ^ string_of_dtype i ^ " " ^ string_of_expr i))

let to_Map m =
    match m with
    JMap m -> m
    |_ -> raise(Failure ("Expected Map got " ^ string_of_dtype m ^ " " ^ string_of_expr m))
    
let to_Int i= 
    match i with
    Int i  -> i
    | _     -> raise(Failure ("Expected Integer got " ^ string_of_dtype i ^ " " ^ string_of_expr i))
    
let is_Int i =
    match i with
    Int i -> true
    |_ -> false
    
let to_String s =
    match s with
    String s -> s
    | _ -> raise(Failure ("Expected String got " ^ string_of_dtype s ^ " " ^ string_of_expr s))