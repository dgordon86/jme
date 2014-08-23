

type bstmt =
    Lit of Datatypes.dtypes (* Push a literal *)
  | Drp           (* Discard a value *)
  | Bin of Ast.op (* Perform arithmetic on top of stack *)
  | Lod of int    (* Fetch global variable *)
  | Str of int    (* Store global variable *)
  | Lfp of int    (* Load frame pointer relative *)
  | Sfp of int    (* Store frame pointer relative *)
  | Jsr of int    (* Call function by absolute address *)
  | Ent of int    (* Push FP, FP -> SP, SP += i *)
  | Rts of int    (* Restore FP, SP, consume formals, push result *)
  | Beq of int    (* Branch relative if top-of-stack is zero *)
  | Bne of int    (* Branch relative if top-of-stack is non-zero *)
  | Bra of int    (* Branch relative *)
  | Vec of int    (* vector init *)
  | Mat of int * int (* matrix init *)
  | Map of int    (* map init *)
  | Lodv of int   (* Load element of vector global *)
  | Lfpv of int   (* Load element of vector local *)
  | Lodm of int   (* Load element of matrix global *)
  | Lfpm of int   (* Load element of matrix local *)
  | Ulvec of int  (* Update element of vector local *)
  | Ugvec of int  (* Update element of vector global *)
  | Ulmat of int  (* Update element of matrix local *)
  | Ugmat of int  (* Update element of matrix global *)
  | Veci          (* create an empty vector and push on to stack *)
  | Mati          (* create an empty matrix and push on to stack *)
  | Hlt           (* Terminate *)

type prog = {
    num_globals : int;   (* Number of global variables *)
    text : bstmt array; (* Code for all the functions *)
  }
  

let string_of_stmt = function
    Lit(i) -> "Lit " ^ Datatypes.string_of_expr i
  | Drp -> "Drp"
  | Bin(Ast.Add) -> "Add"
  | Bin(Ast.Sub) -> "Sub"
  | Bin(Ast.Mult) -> "Mul"
  | Bin(Ast.Div) -> "Div"
  | Bin(Ast.Exponent) -> "Exponent"
  | Bin(Ast.Equal) -> "Eql"
  | Bin(Ast.Neq) -> "Neq"
  | Bin(Ast.Less) -> "Lt"
  | Bin(Ast.Leq) -> "Leq"
  | Bin(Ast.Greater) -> "Gt"
  | Bin(Ast.Geq) -> "Geq"
  | Bin(Ast.Mod) -> "Mod"
  | Lod(i) -> "Lod " ^ string_of_int i
  | Str(i) -> "Str " ^ string_of_int i
  | Lfp(i) -> "Lfp " ^ string_of_int i
  | Sfp(i) -> "Sfp " ^ string_of_int i
  | Jsr(i) -> "Jsr " ^ string_of_int i
  | Ent(i) -> "Ent " ^ string_of_int i
  | Rts(i) -> "Rts " ^ string_of_int i
  | Bne(i) -> "Bne " ^ string_of_int i
  | Beq(i) -> "Beq " ^ string_of_int i
  | Bra(i) -> "Bra " ^ string_of_int i
  | Vec(i) -> "Vect " ^ string_of_int i
  | Mat(i,e) -> "Mat " ^ string_of_int i ^ " " ^ string_of_int e
  | Map(i) -> "Map " ^ string_of_int i
  | Lodv(i) -> "Lodv " ^ string_of_int i
  | Lfpv(i) -> "Lfpv " ^ string_of_int i
  | Lodm(i) -> "Lodm " ^ string_of_int i
  | Lfpm(i) -> "Lfpm " ^ string_of_int i
  | Ulvec(i) -> "Ulvec " ^ string_of_int i
  | Ugvec(i) -> "Ugvec " ^ string_of_int i
  | Ulmat(i) -> "Ulmat " ^ string_of_int i
  | Ugmat(i) -> "Ugmat " ^ string_of_int i
  | Veci -> "Veci"
  | Mati -> "Mati"  
  | Hlt    -> "Hlt"

let string_of_prog p =
  string_of_int p.num_globals ^ " global variables\n" ^
  let funca = Array.mapi
      (fun i s -> string_of_int i ^ " " ^ string_of_stmt s) p.text
  in String.concat "\n" (Array.to_list funca)
