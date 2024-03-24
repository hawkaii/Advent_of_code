(* 
467..114..
...*......
..35..633.
have to index the symbols and save its row and column
    *)



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

let make_matrix lst = 
    let column_len = List.length lst in
    let row_len  = (String.length (List.hd lst)) in

    let mat = ( Array.make_matrix  row_len column_len ' ') in
    let rec helper lst row = 
        match lst with 
        [] -> mat
        | h::[] -> String.iteri (fun i x -> mat.(row).(i)<-x) h; mat
        | h::t -> String.iteri (fun i x -> mat.(row).(i)<-x) h; helper t (row + 1)
    in 
    helper lst (0)




let check_int c = 
    match c with 
    |'0'..'9' ->1
    | '.' -> 0
    | _ -> 2

let acc_int1 arr = 
    let l = Array.length arr in
    let pos = Array.make_matrix (l) (l) " " in 
    let rec form_int i j acc  = 
        if i > (l-1) then acc
        else match arr.(i).(j) with
            | a when check_int a = 1 -> form_int (i) (j+1) (acc^(String.make 1 a))
            | _ -> acc
    in
    let rec place_pos i j = 
        if i > (l-1) then ()
        else if j < (l-1) then
            match arr.(i).(j) with 
            | a when check_int a = 1 -> let wrd = form_int i j "" in
                for y = j to (j + String.length wrd) - 1 do
                pos.(i).(y) <- wrd ;
                done;
                place_pos (i) (j + (String.length wrd) + 1)
            | _ -> place_pos (i) (j+1)
        else place_pos (i+1) 0
    in
    place_pos 0 0;
    pos


let check_direction1 pos (a, b) =
    let check_border arg = arg >= 0 && arg < Array.length pos in
    let rec helper acc (x,y) = 
        if check_border (a+x) && check_border (b+y) then
            match (x,y) with
            | (1,0) ->( 
                match pos.(a+x).(b+y) with
                | " " -> let tmp = 
                    if  check_border (b+y+1) then
                        if check_border (b+y-1) then (pos.(a+x).(b+y-1)::pos.(a+x).(b+y+1)::acc)  
                        else pos.(a+x).(b+y+1)::acc
                    else if check_border (b+y-1) then pos.(a+x).(b+y-1)::acc
                    else acc in helper tmp (-1,0)
                | _ -> helper ((pos.(a+x).(b+y)::acc)) (-1,0)
            )
            | (-1,0) -> (
                match pos.(a+x).(b+y) with
                | " " -> let tmp = 
                    if  check_border (b+y+1) then
                        if check_border (b+y-1) then (pos.(a+x).(b+y-1)::pos.(a+x).(b+y+1)::acc)  
                        else pos.(a+x).(b+y+1)::acc
                    else if check_border (b+y-1) then pos.(a+x).(b+y-1)::acc
                    else acc in helper tmp (0,1)
                | _ -> helper ((pos.(a+x).(b+y)::acc)) (0,1)

            )
            | (0,1) ->  helper ((pos.(a+x).(b+y)::acc)) (0,-1)
            | (0,-1) ->  helper ((pos.(a+x).(b+y)::acc)) (0,0)
            | _ -> acc
        else match (x,y) with 
            | (1,0) -> helper acc (-1,0)
            | (-1 , 0) -> helper acc (0 ,1)
            | (0,1) -> helper acc (0 ,-1)
            | _,_ -> acc
    in helper [] (1,0)

    




(* let make_numlist arr pos = 
    let l = Array.length arr in
    let rec helper i j acc = 
    if i >= (l-1) then acc 
    else if j <= (l-1) then
    match arr.(i).(j) with
    | a when check_int a = 1 -> helper (i) (j+1) acc
    | a when check_int a = 0 -> helper (i) (j+1) acc
    | a -> helper (i) (j+1) (
                print_endline (String.make 1 a);
                (check_direction1 pos (i,j))::acc
                )
    else helper (i+1) 0 acc
    in 
    helper 0 0 []

let add_numlist lst = 
    let add_num acc lst =
        List.fold_left (
            fun y x ->
            match x with 
            " " -> 0 + y
            | _ -> (int_of_string x) + y
        ) acc lst
    in 
    List.fold_left (add_num) 0 lst *)

let check_star c = 
    match c with 
    |'0'..'9' ->1
    | '.' -> 0
    | '*' -> 2
    | _ -> 3


    
(* this function filter all two geared star and multiplies it*)
let filter_2 lst = 
    let tmp_lst = 
        List.map (
            List.filter (fun x -> x <> " ")
        ) lst 
    in 
    let rec mult_2 acc = function 
        | h::t -> if List.length h = 2 then (* This folds the each list and multiplies it by converting to int*)
            let tmp = 
                List.fold_left (
                    fun y x -> (int_of_string x ) * y
                ) 1 h 
            in 
            mult_2 (acc + tmp) t
            else mult_2 acc t
        | [] -> acc
    in mult_2 0 tmp_lst

let make_numlist1 arr pos = 
    let l = Array.length arr in
    let rec helper i j acc = 
        if i >= (l-1) then acc 
        else if j <= (l-1) then
            match arr.(i).(j) with
            | a when check_star a = 2 -> helper (i) (j+1) (
                (check_direction1 pos (i,j))::acc
            )
            | _ -> helper (i) (j+1) acc
        else helper (i+1) 0 acc
    in 
    helper 0 0 []




let () = 
    let mat = read_file "inputs/day3" |> make_matrix in
    let pos = acc_int1 mat in
    let lst = make_numlist1 mat pos in
    print_endline (string_of_int (filter_2 lst)  )
    

