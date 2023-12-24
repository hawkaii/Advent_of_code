(* let string_to_char s = List.init (String.length s) (String.get s)

let rec filternum ls acc = match ls with
|[] -> List.rev acc
|hd::tl-> let a = (Char.code hd) in 
           if a >= 48 && a <= 57 then filternum tl (hd::acc) 
           else filternum tl acc

(* let rec makenum ls ac n = match ls with
|[] -> ac
| h::tl -> makenum tl ((((int_of_char h)-48) * (int_of_float(10. ** n))) + ac) (n +. 1.) *)

let char_to_int a = int_of_char a - 48
let two_of_ls ls = (char_to_int(List.hd ls ),char_to_int( List.hd (List.rev ls)))

let makenum (x,y) = x*10 + y

let int_of_st s = makenum (two_of_ls (filternum (string_to_char s) [] ))
;; *)


let match_int str = 
  match str with
  | s when String.starts_with s ~prefix:"one" -> 1
  | s when String.starts_with s ~prefix:"1" -> 1
  | s when String.starts_with s ~prefix:"two" -> 2
  | s when String.starts_with s ~prefix:"2" -> 2
  | s when String.starts_with s ~prefix:"three" -> 3
  | s when String.starts_with s ~prefix:"3" -> 3
  | s when String.starts_with s ~prefix:"four" -> 4
  | s when String.starts_with s ~prefix:"4" -> 4
  | s when String.starts_with s ~prefix:"five" -> 5
  | s when String.starts_with s ~prefix:"5" -> 5
  | s when String.starts_with s ~prefix:"six" -> 6
  | s when String.starts_with s ~prefix:"6" -> 6
  | s when String.starts_with s ~prefix:"seven" -> 7
  | s when String.starts_with s ~prefix:"7" -> 7
  | s when String.starts_with s ~prefix:"eight" -> 8
  | s when String.starts_with s ~prefix:"8" -> 8
  | s when String.starts_with s ~prefix:"nine" -> 9
  | s when String.starts_with s ~prefix:"9" -> 9
  |_ -> -1


let rec position_of_int str x acc = 
  let slen = String.length str in 
  if x < slen then 
    position_of_int str (x+1) (
      (x ,match_int (String.sub str x (slen - x)))::acc)
  else
    acc


let rec find ls = match ls with
|(_, -1)::tl ->find tl
  |(_,y)::_ -> y
  | _ -> -1

let make_num a b = a*10 + b

let final_num str =  let ls = position_of_int str 0 [] in make_num (find (List.rev ls)) (find ls)  

let rec collectnum accum  = match In_channel.input_line In_channel.stdin with
Some "" -> accum
|Some v -> collectnum (accum + (final_num v)) 
|None -> accum

let () = print_endline (string_of_int (collectnum 0)  )
