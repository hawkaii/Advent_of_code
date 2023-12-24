(*
have to parse the string to count colors from a string written in the format 
Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
*)

let parse_string string = 
    let split1 = String.split_on_char ':' string in
    match split1 with
    |[] ->(0, [])
    |head::tl -> 
        let game_no =int_of_string ( List.hd (List.tl (String.split_on_char ' ' head))) in
        let split = String.split_on_char ';' (List.hd tl) in
        let rec parse_list acc= function
            | [] -> acc
            | h::t ->
                parse_list ((String.split_on_char ',' h)::acc) t in 
        (game_no,List.concat  (parse_list [] split))
(*

output
parse_string "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green" ;;
- : int * string list =
(1, [" 2 green"; " 1 red"; " 2 green"; " 6 blue"; " 3 blue"; " 4 red"])
*)
let rec parse_colors stringlist acc=
    match stringlist with
    | [] -> acc
    | h::t ->let count_colors = (String.split_on_char ' ' (String.trim h )  )in
        parse_colors t (count_colors::acc)

(* Output *)
(* parse_colors x [];;
- : string list list =
[["4"; "red"]; ["3"; "blue"]; ["6"; "blue"]; ["2"; "green"]; ["1"; "red"];
    ["2"; "green"]] *)
(* let rec count_colors ls = let arr = Array.make 3 0. in  *)
(*     match ls with  *)
(*     [] ->  arr *)
(*     | h::t -> (match (List.hd(List.tl h))   with *)
(*     | "blue" -> arr.(0) <- (arr.(0) +. Float.of_string (List.hd h)) *)
(*     | "red" -> arr.(1) <- (arr.(1) +. Float.of_string (List.hd h)) *)
(*     | "green" -> arr.(2) <- (arr.(2) +. Float.of_string (List.hd h)) *)
(*     | _ -> () *)
(*     );print_endline (List.hd h);count_colors t  *)

let  count_colors ls = 
    let arr = Array.make 3 0. in 
    let rec helper s =
        match  s with
        | [] -> arr
        | [num;color]::t -> (
            match color with
            | "blue" -> arr.(0) <- (arr.(0) +. Float.of_string num)
            | "red" -> arr.(1) <- (arr.(1) +. Float.of_string num)
            | "green" -> arr.(2) <- (arr.(2) +. Float.of_string num)
            | _ -> ()
        );  helper t  
        | _ -> failwith "Invalid input"
    in
    helper  ls

let filter_round arr = 
    let boundary = Array.make 3 0. in
    boundary.(0) <- 14.;
    boundary.(1) <- 12.;
    boundary.(2) <- 13.;
    arr.(0) <= boundary.(0) && arr.(1)  <= boundary.(1) && arr.(2)  <= boundary.(2)

let  filter_game string = 
    let (game_no,colors) = parse_string string in
    let ans = parse_colors colors [] |> count_colors |> filter_round
    in (game_no,ans)

(*
let rec collectnum accum  = match In_channel.input_line In_channel.stdin with
    Some "" -> accum
    |Some v -> let (game_no,ans) = filter_game v in 
        if ans then collectnum (accum + game_no) else collectnum accum
    |None -> accum

let () = print_endline (string_of_int (collectnum 0)  ) *)


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

let rec collectnum accum  ls =match ls  with
    [] -> accum
    | ""::[] -> accum
    | h::t-> let (game_no,ans) = filter_game h in 
    (*     print_endline (string_of_int game_no); *)
        if ans then collectnum (accum + game_no) t else collectnum accum t

let () = print_endline (string_of_int (collectnum 0 (read_file "inputs/game"))  ) 
