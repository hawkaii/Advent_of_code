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
(* let parse_string string = 
    let split1 = String.split_on_char ':' string in
    match split1 with
    |[] ->(0,([],[]))
    |head::tl -> 
        let game_no = try int_of_string (List.hd (List.tl (String.split_on_char ' ' head)))
                       with Failure _ -> (print_endline ("Failed to parse game number from: " ^ string); 0) in
        let split = String.split_on_char ';' (List.hd tl) in
        let parsed_list = (String.split_on_char ' ' (List.hd split )) in 
        let rec separate_cards acc1 acc2 = function 
            |[] -> (acc1,acc2)
            |""::t -> separate_cards acc1 acc2 t
            |"|"::t -> let rec push_acc2 acc3 = function
                | [] -> separate_cards acc1 acc3 []
                |""::t -> push_acc2 acc3 t
                | h::t -> try
                            let int_val = int_of_string h in
                            push_acc2 (int_val::acc3) t
                          with Failure _ -> (print_endline ("Failed to parse card value from: " ^ h); push_acc2 acc3 t)
                in push_acc2 acc2 t
            |h::t -> try
                          let int_val = int_of_string h in
                          separate_cards (int_val::acc1) acc2 t
                        with Failure _ -> (print_endline ("Failed to parse card value from: " ^ h); separate_cards acc1 acc2 t)
        in 
        (game_no,(separate_cards [] [] parsed_list)) *)



let parse_list lst =
let rec helper acc = function 
    | [] -> acc 
    | h::tl -> let parsed = (parse_string1 h) in helper (parsed::acc) tl
    in (helper [] lst) |> List.rev

(* this function  calculates the point given a wintlist and a cardlist *)
let calculate_points winlist cardlist = 
    let rec helper count wl =
    match wl with 
        |[] -> 2. ** (float_of_int count) 
        |h::t -> if (List.exists (fun x -> x = h) cardlist) then helper (count+1) t
            else helper count t
    in helper (-1) winlist


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
    List.fold_left filter_points 0. lst

let () = 
    let ans = read_file "inputs/day4" |> parse_list |> total_count |> add_total in
    Printf.printf "%f" ans


