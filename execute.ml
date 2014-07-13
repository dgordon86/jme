open Ast
open Bytecode
open Datatypes

(* Stack layout just after "Ent":

              <-- SP
   Local n
   ...
   Local 0
   Saved FP   <-- FP
   Saved PC
   Arg 0
   ...
   Arg n *)
   


let execute_prog prog =
  let stack = Array.make 1024 Null
  and globals = Array.make prog.num_globals Null in

  let rec exec fp sp pc = match prog.text.(pc) with
    Lit i  -> stack.(sp) <- i; exec fp (sp+1) (pc+1)
  | Drp    -> exec fp (sp-1) (pc+1)
  | Bin op -> let op1 = stack.(sp-2) and op2 = stack.(sp-1) in     
      stack.(sp-2) <- (let boolean i = if i then Int 1 else Int 0 in
      match op with
	Add     -> add op1 op2
      | Sub     -> subtract op1 op2
      | Mult    -> multiply op1 op2
      | Div     -> divide op1 op2
      | Equal   -> equal op1 op2
      | Neq     -> boolean (op1 != op2)
      | Less    -> boolean (op1 <  op2)
      | Leq     -> boolean (op1 <= op2)
      | Greater -> boolean (op1 >  op2)
      | Geq     -> boolean (op1 >= op2)) ;
      exec fp (sp-1) (pc+1)
  | Lod i   -> stack.(sp)   <- globals.(i)  ; exec fp (sp+1) (pc+1)
  | Str i   -> globals.(i)  <- stack.(sp-1) ; exec fp sp     (pc+1)
  | Lfp i   -> stack.(sp)   <- stack.(fp+i) ; exec fp (sp+1) (pc+1)
  | Sfp i   -> stack.(fp+i) <- stack.(sp-1) ; exec fp sp     (pc+1)
  | Jsr(-1) -> print_endline (string_of_expr stack.(sp-1)) ; exec fp sp (pc+1)
  | Jsr i   -> stack.(sp)   <- Int (pc + 1)       ; exec fp (sp+1) i
  | Ent i   -> stack.(sp)   <- Int fp           ; exec sp (sp+i+1) (pc+1)
  | Rts i   -> let new_fp = cast_int stack.(fp) and new_pc = cast_int stack.(fp-1) in
               stack.(fp-i-1) <- stack.(sp-1) ; exec new_fp (fp-i) new_pc
  | Beq i   -> exec fp (sp-1) (pc + if stack.(sp-1) = (Int 0) then i else 1)
  | Bne i   -> exec fp (sp-1) (pc + if stack.(sp-1) != (Int 0) then i else 1)
  | Bra i   -> exec fp sp (pc+i)
  | Hlt     -> ()

  in exec 0 0 0
