open Ast
open Bytecode
open Datatypes
open Util

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

module StringMap = Map.Make(String)

let execute_prog prog =
  let stack = Array.make 1024 Null
  and globals = Array.make prog.num_globals Null in
  
  
  let fillMatrix x y top = 
    let m = Array.make_matrix x y Null in
        for i = 0 to x -1 do
            for j = 0 to y -1 do
                m.(i).(j) <- stack.(top + (i*(y) +j))
            done;
        done;
        m
  in
  
  let fillVector n top =
    if n == 0 then Array.make 0 Null
        else let v = Array.make n Null in
            for i = 0 to n - 1 do
                v.(i) <- stack.(top - ((n-1) - i))
            done;
            v in
  let rec fillMap t n map =
    if n == t then map
        else
            fillMap (t+2) (n) (StringMap.add (to_String stack.(t+1)) stack.(t) map) in

  let rec exec fp sp pc = match prog.text.(pc) with
    Lit i  -> stack.(sp) <- i; exec fp (sp+1) (pc+1)
  | Drp    -> exec fp (sp-1) (pc+1)
  | Bin op -> let op1 = stack.(sp-2) and op2 = stack.(sp-1) in     
      stack.(sp-2) <- (
      match op with
	Add     -> add op1 op2
      | Sub     -> subtract op1 op2
      | Mult    -> multiply op1 op2
      | Div     -> divide op1 op2
      | Exponent -> power op1 op2
      | Equal   -> equal op1 op2
      | Neq     -> nequal op1 op2
      | Less    -> lessthn op1 op2
      | Leq     -> lessthneq op1 op2
      | Greater -> greatthn op1 op2
      | Geq     -> greatthneq op1 op2
      | Mod     -> remainder op1 op2) ;
      exec fp (sp-1) (pc+1)
  | Lod i   -> stack.(sp)   <- globals.(i)  ; exec fp (sp+1) (pc+1)
  | Str i   -> globals.(i)  <- stack.(sp-1) ; exec fp sp     (pc+1)
  | Lfp i   -> stack.(sp)   <- stack.(fp+i) ; exec fp (sp+1) (pc+1)
  | Sfp i   -> stack.(fp+i) <- stack.(sp-1) ; exec fp sp     (pc+1)
  | Jsr(-1) -> print_endline (string_of_expr stack.(sp-1)) ; exec fp sp (pc+1)
  | Jsr(-2) -> stack.(sp-1) <- getWidth stack.(sp-1); exec fp sp (pc+1)
  | Jsr(-3) -> stack.(sp-1) <- getHeight stack.(sp-1); exec fp sp (pc+1)
  | Jsr(-4) -> stack.(sp-1) <- Float (sqrt (to_Float stack.(sp-1))); exec fp sp (pc+1)
  | Jsr(-5) -> sortVect stack.(sp-1); exec fp sp (pc+1)
  | Jsr(-6) -> stack.(sp-1) <- keyExists stack.(sp-2) stack.(sp-1); exec fp sp (pc+1)
  | Jsr(-7) -> stack.(sp-1) <- Boolean( is_Int stack.(sp-1)); exec fp sp (pc+1)
  | Jsr(-8) -> stack.(sp-1) <- Boolean( is_Float stack.(sp-1)); exec fp sp (pc+1)
  | Jsr(-9) -> stack.(sp-1) <- Boolean( is_Bool stack.(sp-1)); exec fp sp (pc+1)
  | Jsr(-10) -> stack.(sp-1) <- Boolean( is_String stack.(sp-1)); exec fp sp (pc+1)
  | Jsr(-11) -> stack.(sp-1) <- Boolean( is_Vector stack.(sp-1)); exec fp sp (pc+1)
  | Jsr(-12) -> stack.(sp-1) <- Boolean( is_Matrix stack.(sp-1)); exec fp sp (pc+1)
  | Jsr(-13) -> stack.(sp-1) <- Boolean( is_JMap stack.(sp-1)); exec fp sp (pc+1)
  | Jsr i   -> stack.(sp)   <- Int (pc + 1)       ; exec fp (sp+1) i
  | Ent i   -> stack.(sp)   <- Int fp           ; exec sp (sp+i+1) (pc+1)
  | Rts i   -> let new_fp = to_Int stack.(fp) and new_pc = to_Int stack.(fp-1) in
               stack.(fp-i-1) <- stack.(sp-1) ; exec new_fp (fp-i) new_pc
  | Beq i   -> exec fp (sp-1) (pc + if to_Bool(equal stack.(sp-1) (Boolean false)) then i else 1)
  | Bne i   -> exec fp (sp-1) (pc + if to_Bool(nequal stack.(sp-1) (Boolean false)) then i else 1)
  | Bra i   -> exec fp sp (pc+i)
  | Vec i -> stack.(sp-i) <- Vector(fillVector i (sp -1)); exec fp (sp-i +1) (pc+1)
  | Mat (x,y) -> stack.(sp - (x*y)) <- Matrix( fillMatrix x y (sp - x*y)); exec fp (sp - x*y +1) (pc+1)
  | Veci -> stack.(sp-1) <- Vector(Array.make (to_Int(stack.(sp-1))) Null); exec fp sp (pc+1)
  | Mati -> stack.(sp-2) <- Matrix(Array.make_matrix (to_Int(stack.(sp-2))) (to_Int(stack.(sp-1))) Null);exec fp (sp-1) (pc+1) 
  | Map i -> stack.(sp - 2*i) <- JMap(fillMap (sp-2*i ) sp StringMap.empty); exec fp (sp-2*i+1) (pc+1)
  | Lodv i -> let nth = stack.(sp-1) in
                stack.(sp-1) <- getVectElement nth globals.(i); exec fp (sp) (pc+1)
  | Lfpv i -> let nth = stack.(sp-1) in
                stack.(sp-1) <- getVectElement nth stack.(fp+i); exec fp (sp) (pc+1)
  | Lfpm i -> let x = stack.(sp-2) in 
              let y = stack.(sp-1) in
                stack.(sp-2) <- getMatxElement x y stack.(fp+i); exec fp (sp -1) (pc+1)
  | Lodm i -> let x = stack.(sp-2) in
              let y = stack.(sp-1) in
                stack.(sp-2) <- getMatxElement x y globals.(i); exec fp (sp -1) (pc+1)
  | Ulvec i -> if is_Int (stack.(sp-1)) then  updateVectElement stack.(sp-1) stack.(fp+i) stack.(sp-2)
                    else  stack.(fp+i) <- JMap((StringMap.add (to_String(stack.(sp-1))) stack.(sp-2) (to_JMap(stack.(fp+i)))));
                 exec fp sp (pc+1)
  | Ugvec i -> if is_Int (stack.(sp-1)) then  updateVectElement stack.(sp-1) globals.(i) stack.(sp-2)
                    else  globals.(i) <- JMap((StringMap.add (to_String(stack.(sp-1))) stack.(sp-2) (to_JMap(globals.(i)))));
  
                
                (*let indx = stack.(sp-1) and nval = stack.(sp-2) and svec = (to_Vector globals.(i)) in 
                svec.(to_Int indx) <- nval;*)
                 exec fp sp (pc+1)
  | Ulmat i -> let x = stack.(sp-2) and y = stack.(sp-1) and nval = stack.(sp-3) and svec = (to_Matrix stack.(fp+i)) in 
                svec.(to_Int x).(to_Int y) <- nval;
                 exec fp sp (pc+1)
  | Ugmat i -> let indx = stack.(sp-1) and nval = stack.(sp-2) and svec = (to_Vector globals.(i)) in 
                svec.(to_Int indx) <- nval;
                 exec fp sp (pc+1)
  | Hlt     -> ()

  in exec 0 0 0
