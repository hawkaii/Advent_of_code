(* 
467..114..
...*......
..35..633.
have to index the symbols and save its row and column
    *)

let check_int c = 
    match c with 
    |'0'..'9' ->1
    | '.' -> 0
    | _ -> 2

let index_symbols str =
    let lst =( String.to_seqi str) |> List.of_seq in
    let rec helper acc1 acc2 = function
        | [] -> (acc1,acc2)
        | (i,c)::t ->
            match check_int c with
            | 0 -> helper acc1 acc2 t
            | 1 -> helper ((i,c)::acc1) acc2 t
            | _ -> helper acc1 ((i,c)::acc2) t

    in
    helper [] [] lst

 let read_file filename =
 let input_channel = open_in filename in
 let rec read_lines lines =
   try
     let line = input_line input_channel in
     read_lines (line :: lines)
   with End_of_file ->
     close_in input_channel;
     List.rev lines
 in
 read_lines []

let rec collectnum accum  ls row = match ls  with
    [] -> accum
    | ""::[] -> accum
    | h::t-> 


let () = print_endline (string_of_int (collectnum 0 (read_file "inputs/game"))  ) 
    
