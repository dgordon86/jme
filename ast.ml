type op = Add | Sub | Mult | Div | Exponent | Equal | Neq | Less | Leq | Greater | Geq | Mod
 
 
type expr =
    Literal of int
  | Float of float
  | Boolean of bool
  | String of string
  | Id of string
  | Binop of expr * op * expr
  | Assign of string * expr
  | Call of string * expr list
  | VectAssign of string * expr * expr
  | VectRef of string * expr
  | VectorInit of expr
  | Matrix of expr list list
  | MatrixInit of expr * expr
  | MatxRef of string * expr * expr
  | MatxAssign of string * expr * expr * expr
  | JMap of (expr * expr ) list
  | Noexpr

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt
  

type func_decl = {
    fname : string;
    formals : string list;
    locals : string list;
    body : stmt list;
  }

type program = string list * func_decl list

let rec string_of_expr = function
    Literal(l) -> string_of_int l
  | Float(f) -> string_of_float f
  | Boolean(b) -> string_of_bool b
  | String(s) -> s
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^
      (match o with
	Add -> "+" | Sub -> "-" | Mult -> "*" | Div -> "/"
      | Mod -> "%"
      | Exponent -> "^"
      | Equal -> "==" | Neq -> "!="
      | Less -> "<" | Leq -> "<=" | Greater -> ">" | Geq -> ">=") ^ " " ^
      string_of_expr e2
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Matrix (m) -> "\n[" ^ (String.concat ";\n " (List.map (fun lexpr -> "" ^ (String.concat "," (List.map (fun e -> string_of_expr e) lexpr ))  ^ "" ) m))
                ^ "]"
  | JMap (m) -> "|" ^ String.concat ", " (List.map (fun (k,v) -> string_of_expr k ^ "=>" ^ string_of_expr v) m) ^ "|"
  | VectorInit(e) -> "new " ^ "[" ^ (string_of_expr e) ^ "]"
  | MatrixInit(x,y) -> "new " ^ "[" ^ (string_of_expr x) ^ "] [" ^ (string_of_expr y) ^ "]"
  | MatxRef(v, x, y) -> v ^ "[" ^ string_of_expr x ^ "][" ^ string_of_expr y ^ "]"
  | VectRef(v, e) -> v ^ "[" ^ string_of_expr e ^ "]"
  | VectAssign(v, e1, e2) -> v ^ "[" ^ string_of_expr e1 ^ "] = " ^ string_of_expr e2
  | MatxAssign(m, e1, e2, e3) -> m ^ "[" ^ string_of_expr e1 ^ "][" ^ string_of_expr e2 ^ "] = " ^ string_of_expr e3
  | Noexpr -> ""

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s

let string_of_vdecl id = "var " ^ id ^ ";\n"

let string_of_fdecl fdecl =
  fdecl.fname ^ "(" ^ String.concat ", " fdecl.formals ^ ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)
