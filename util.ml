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
    
let buf_len = 8192

let input_all ic =
  let rec loop acc total buf ofs =
    let n = input ic buf ofs (buf_len - ofs) in
    if n = 0 then
      let res = String.create total in
      let pos = total - ofs in
      let _ = String.blit buf 0 res pos ofs in
      let coll pos buf =
        let new_pos = pos - buf_len in
        String.blit buf 0 res new_pos buf_len;
        new_pos in
      let _ = List.fold_left coll pos acc in
      res
    else
      let new_ofs = ofs + n in
      let new_total = total + n in
      if new_ofs = buf_len then
        loop (buf :: acc) new_total (String.create buf_len) 0
      else loop acc new_total buf new_ofs in
  loop [] 0 (String.create buf_len) 0