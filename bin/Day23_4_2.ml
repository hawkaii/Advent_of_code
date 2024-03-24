
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

let parse_string1 string = 
    let split1 = String.split_on_char ':' string in
    match split1 with
    |[] ->(([],[]))
    |_::tl -> 
        (* let game_no =int_of_string ( List.hd (List.tl (String.split_on_char ' ' head))) in *)
        let split = String.split_on_char ';' (List.hd tl) in
        let parsed_list = (String.split_on_char ' ' (List.hd split ))  in 
        let rec separate_cards  acc1 acc2 = function 
            |[] -> (acc1,acc2)
            |""::t -> separate_cards acc1 acc2 t
            |"|"::t -> let rec push_acc2 acc3 = function
                | [] -> separate_cards acc1 acc3 []
                |""::t -> push_acc2 acc3 t
                | h::t -> push_acc2 ((int_of_string h)::acc3) t
                in push_acc2 acc2 t
            |h::t -> separate_cards ((int_of_string h)::acc1) acc2 t
        in 
        ((separate_cards [] [] parsed_list))


let parse_list lst =
let rec helper acc = function 
    | [] -> acc 
    | h::tl -> let parsed = (parse_string1 h) in helper (parsed::acc) tl
    in (helper [] lst) |> List.rev

(* this function  calculates the point given a wintlist and a cardlist *)
let calculate_points winlist cardlist = 
    let rec helper count wl =
    match wl with 
        |[] ->  count  
        |h::t -> if (List.exists (fun x -> x = h) cardlist) then helper (count+1) t
            else helper count t
    in helper (0) winlist


(* this  function make an array according to  the new rules for scratch card
   which is to repeat the following cards the same point a card wins
   *)

let new_game lst = 
    let l = List.length lst in
    let arr = Array.make l  1 in 
    let rec helper i = function 
        | [] -> arr
        | (wl,cl)::t ->  let scards = calculate_points wl cl in 
                for j = (i+1) to (i+scards) do
                    Printf.printf "i = %d j = %d scards = %d \n" i j scards;
                    if (j < l) then arr.(j) <- arr.(j) + arr.(i)
                done;
            helper (i+1) t
    in helper 0 lst


let add_array = Array.fold_left (+) 0
(*
(* this function makes a point list with game no in a tuple *)
let total_count lst = 
    let rec helper acc = function 
        | [] -> acc
        | (wl,cl)::t -> helper (((calculate_points wl cl))::acc) t
    in
    helper [] lst

(* this function adds the tupled point list *)
let add_total lst = 
    let  filter_points acc (points) = if points >= 1.0 then acc +. points else acc
    in
    List.fold_left filter_points 0. lst *)

let () = 
    let ans = read_file "inputs/day4" |> parse_list |> new_game |> add_array in
    Printf.printf "%d" ans


