(*1 subtask*)
let rec member c t l = match l with 
    | [] -> false
    | h :: tl -> if c t h = 0 then true else member c t tl

let equal_second_components (_, x) (_, y) = compare x y 
let evens_eq_evens_odds_eq_odds n1 n2 = compare (n1 mod 2) (n2 mod 2)


(*2 subtask*)
let rec count el lst = match lst with
        | [] -> 0
        | h :: t -> if h = el then 1 + count el t else count el t
      
let count_occurrences lst1 =
let rec count_occurrences_unordered lst1  = match lst1 with
| [] -> []
| h :: t -> 
  let occurrences = count h t in
  if occurrences > -1 then 
     ((h, occurrences + 1) :: count_occurrences_unordered (List.filter (fun x -> x <> h) t))
  else (count_occurrences_unordered t) in
(List.sort (fun (a, x) (b, y) -> match compare y x with
|0 -> compare a b
|c -> c) (count_occurrences_unordered lst1))


(*3 subtask*)
let rec drop_Last lst = match lst with
    | [] -> failwith "Empty list has no last element"
    | [_] -> []
    | h :: t -> h :: (drop_Last t)

(*4 subtask*)
let rec drop_last_opt lst = match lst with
    | [] -> None
    | [_] -> Some []
    | h :: t ->
       match drop_last_opt t with 
       | None -> None
       | Some t -> Some (h :: t)

(*5 subtask*)
let rec zip_with map lst1 lst2 = 
    match lst1, lst2 with
    | [], lst2-> []
    | lst1, [] -> []
    | h :: t, h2 :: t2 -> map h h2 :: zip_with map t t2

(*6 subtask*)
(* let unzip lst = 
   match lst with
   | [] -> ([], [])
   | (x, y) :: t ->
   let first_elems = List.map (fun (x, _) -> x) lst in
   let second_elems = List.map (fun (_, y) -> y) lst in
    (first_elems, second_elems) *)

    let unzip lst =
      List.fold_left
        (fun (xs, ys) (x, y) -> (x :: xs, y :: ys))
        ([], []) lst  |> fun (xs, ys) -> (List.rev xs, List.rev ys)

    

(*7 subtask*)



(* The evaluation steps of unzip [('a',1);('b',2)] are as follows:

The expression unzip [('a',1);('b',2)] is evaluated.
The pattern lst in the definition of unzip is matched with the argument [('a',1);('b',2)].
Since the argument is not an empty list, the pattern (x, y) :: t matches the first element (x, y) with ('a', 1) and the rest t with [('b', 2)].
The expression List.map (fun (x, _) -> x) lst is evaluated with lst being [('a',1);('b',2)].
The anonymous function (fun (x, _) -> x) takes each tuple in the list and returns its first element. Thus, the expression evaluates to ['a'; 'b'].
The expression List.map (fun (_, y) -> y) lst is evaluated with lst being [('a',1);('b',2)].
The anonymous function (fun (_, y) -> y) takes each tuple in the list and returns its second element. Thus, the expression evaluates to [1; 2].
The tuple (first_elems, second_elems) is constructed with first_elems being ['a'; 'b'] and second_elems being [1; 2].
The final result of the expression is (['a'; 'b'], [1; 2]). *)




(* The List.fold_left function takes an accumulator tuple (xs, ys) and a pair (x, y) 
as input, and returns a new accumulator tuple with x added to the first element list xs, 
and y added to the second element list ys. The function is applied to the initial accumulator 
value ([], []) and the input list lst.
Note that the resulting lists are reversed because the fold function accumulates elements 
from the left, while List.map accumulates them from the right. To correct this, we use the List.rev function
 to reverse the resulting lists before returning them. *)

(*8 subtask*)

(* assuming every player only scored 1 goal, calculating the tuple for wins losses draws and scored goals*)
type team = Arg | Sau | Mex | Pol | Por | Kor | Uru | Gha 

(*returns the lst in this format  (team * int * int * int * int * int * int * int) list *)

(* let team1 lst = List.map (fun (x, lst1, y, lst2) -> x) lst (*returns the list of the first teams*)
let team2 lst = List.map (fun (x, lst1, y, lst2) -> y) lst (*returns the list of the second teams*)
let team_names = team1 lst @ team2 lst (*concatinates that list*)
let teams = List.sort_uniq compare team_names  (*removes duplicates*)

let final_team_names lst = List.sort_uniq compare(List.concat (List.map (fun (x, lst1, y, lst2) -> [x; y]) lst )) *)

let my_teams lst = List.sort_uniq compare (List.concat (List.map (fun (team1, player_list1, team2, player_list2) -> [team1; team2]) lst))


let rec games_played team list = match list with  (*miutiteb teams da itvlis ramdeni tamashi itamasha jamshi(mxolod pirvel an meore gundshi)*)
  | [] -> 0
  | (x, lst1, y, lst2)  :: t -> if x = team then 1 + games_played team t else if y = team then 1 + games_played team t else
     games_played team t

let rec wins team lst = match lst with
  | [] -> 0
  | (x, lst1, y, lst2) :: t -> if (x = team && (List.length lst1 - List.length lst2) > 0) 
    || (y = team && (List.length lst1 - List.length lst2) < 0)
    then 1 + wins team t
else wins team t

let rec draws team lst = match lst with 
  | [] -> 0
  | (x, lst1, y, lst2) :: t -> if (x = team && (List.length lst1 = List.length lst2)) 
    || (y = team && (List.length lst1 = List.length lst2)) 
  then 1 + draws team t
else draws team t

let rec loses team lst = match lst with 
  | [] -> 0
  | (x, lst1, y, lst2) :: t -> if (x = team && (List.length lst1 - List.length lst2) < 0) 
    || (y = team && (List.length lst1 - List.length lst2) > 0)
    then 1 + loses team t
else loses team t

let rec scored_goals1 team lst = match lst with 
  | [] -> 0
  | (x, lst1, y, lst2) :: t -> if x = team then List.length lst1 + scored_goals1 team t else scored_goals1 team t

let rec scored_goals2 team lst = match lst with 
  | [] -> 0
  | (x, lst1, y, lst2) :: t -> if y = team then List.length lst2  + scored_goals2 team t else scored_goals2 team t

let rec goals_against1 team lst = match lst with 
| [] -> 0
| (x, lst1, y, lst2) :: t -> if x = team then List.length lst2 + goals_against1 team t else goals_against1 team t

let rec goals_against2 team lst = match lst with 
| [] -> 0
| (x, lst1, y, lst2) :: t -> if y = team then List.length lst1 + goals_against2 team t else goals_against2 team t

(* let rec points team lst = match lst with 
  | [] -> 0
  | (x, lst1, y, lst2) :: t -> if (x = team || y = team) then (3 * (wins team lst) + draws team lst) + points team t else points team t *)

let points team list = (wins team list * 3) + draws team list

let result1 my_teams lst = List.map (fun t -> (t, games_played t lst, wins t lst, draws t lst, loses t lst,
 (scored_goals1 t lst + scored_goals2 t lst), 
(goals_against1 t lst + goals_against2 t lst), points t lst)) (my_teams lst)




(* returns the list in this format  (string * team * int) list *)

let players_by_team lst team = let empty_list = [] in List.concat 
(List. map (fun (team1, player_list1, team2, player_list2) -> if team1 = team then player_list1 @ empty_list else 
if team2 = team then player_list2 @ empty_list else [] @ empty_list) lst)

let my_players lst = List.sort_uniq compare (List.concat (List.map (fun t -> (players_by_team lst t)) (my_teams lst)))
let players_duplicates lst = List.concat (List.map (fun t -> (players_by_team lst t)) (my_teams lst))
(* let rec team_by_player lst p =
  match lst with
  | [] -> []
  | (team1, player_list1, team2, player_list2) :: t ->
      if List.mem p player_list1 then team1 
      else if List.mem p player_list2 then team2
      else team_by_player t p *)

let rec team_by_player lst p : team =
  match lst with
  | [] -> failwith "Player not found in any team"
  | (team1, player_list1, team2, player_list2) :: t ->
      if List.mem p player_list1 then team1 
      else if List.mem p player_list2 then team2
      else team_by_player t p


let rec count el lst = match lst with
        | [] -> 0
        | h :: t -> if h = el then 1 + count el t else count el t

let result2 my_players lst = List.map (fun p -> (p, team_by_player lst p, count p (players_duplicates lst))) (my_players lst)


(*sorts the first list*)

let rec insert1 tup result1 = match result1 with
  | [] -> [tup]
  | (t, g, w, d, l, gf, ga, p) :: tl -> 
  let (team, gp, wins, draws, loses, goals_for, goals_against, points) = tup in 
  if points > p then tup :: (t, g, w, d, l, gf, ga, p) :: tl 
  else if points < p then (t, g, w, d, l, gf, ga, p) :: insert1 tup tl
  else if (goals_for - goals_against) > (gf - ga) then  tup :: (t, g, w, d, l, gf, ga, p) :: tl 
  else if (goals_for - goals_against) < (gf - ga) then (t, g, w, d, l, gf, ga, p) :: insert1 tup tl
  else if goals_for > gf then tup :: (t, g, w, d, l, gf, ga, p) :: tl 
  else if goals_for < gf then (t, g, w, d, l, gf, ga, p) :: insert1 tup tl
  else tup :: (t, g, w, d, l, gf, ga, p) :: tl 


  let rec sorted_list1 list = match list with
  | [] -> []
  | h :: t -> insert1 h (sorted_list1 t)


(*sorts the second list*)
let rec insert2 tup result2 = match result2 with
    | [] -> [tup]
    | (name1, team1, goal1) :: t -> 
    let (name2, team2, goal2) = tup in
    if goal2 > goal1 then tup :: (name1, team1, goal1) :: t 
    else if goal2 <  goal1 then (name1, team1, goal1) :: insert2 tup t
    else if goal2 = goal1 && String.compare name1 name2 < 0 then (name1, team1, goal1) :: insert2 tup t
    else tup :: (name1, team1, goal1) :: t

let rec sorted_list2 list = match list with
  | [] -> []
  | h :: t -> insert2 h (sorted_list2 t)


(*the final function (concatination of both sorted lists) *)

let table_and_scorers lst = (sorted_list1 (result1 my_teams lst) , sorted_list2 (result2 my_players lst))


let wc22_C = 
  [(Arg, ["Messi"], Sau, ["Al-Shehri"; "Al-Dawsari"]);
   (Mex, [], Pol, []);
   (Pol, ["Zielinski"; "Lewandowski"], Sau, []);
   (Arg, ["Messi"; "Fernandez"], Mex, []);
   (Pol, [], Arg, ["Mac Allister"; "Alvarez"]);
   (Sau, ["Al-Dawsari"], Mex, ["Martin"; "Chavez"])
  ]
  
let wc22_H = 
  [(Uru, [], Kor, []);
   (Por, ["Ronaldo"; "Felix"; "Leao"], Gha, ["Ayew"; "Bukari"]);
   (Kor, ["Cho Gue-sung"; "Cho Gue-sung"], Gha, ["Salisu"; "Kudus"; "Kudus"]);
   (Por, ["Fernandes"; "Fernandes"], Uru, []);
   (Kor, ["Kim Young-gwon"; "Hwang Hee-chan"], Por, ["Horta"]);
   (Gha, [], Uru, ["De Arrascaeta"; "De Arrascaeta"])
  ]

let testing_table_and_scorers () =
    let l =
      [
        __LINE_OF__ (table_and_scorers wc22_H =
                       ([(Por, 3, 2, 0, 1, 6, 4, 6);
                         (Kor, 3, 1, 1, 1, 4, 4, 4);
                         (Uru, 3, 1, 1, 1, 2, 2, 4);
                         (Gha, 3, 1, 0, 2, 5, 7, 3)],
                        [("Cho Gue-sung", Kor, 2);
                         ("De Arrascaeta", Uru, 2);
                         ("Fernandes", Por, 2);
                         ("Kudus", Gha, 2);
                         ("Ayew", Gha, 1);
                         ("Bukari", Gha, 1);
                         ("Felix", Por, 1);
                         ("Horta", Por, 1);
                         ("Hwang Hee-chan", Kor, 1);
                         ("Kim Young-gwon", Kor, 1);
                         ("Leao", Por, 1);
                         ("Ronaldo", Por, 1);
                         ("Salisu", Gha, 1)]));
        __LINE_OF__ (table_and_scorers wc22_C =
                       ([(Arg, 3, 2, 0, 1, 5, 2, 6);
                         (Pol, 3, 1, 1, 1, 2, 2, 4);
                         (Mex, 3, 1, 1, 1, 2, 3, 4);
                         (Sau, 3, 1, 0, 2, 3, 5, 3)],
                        [("Al-Dawsari", Sau, 2);
                         ("Messi", Arg, 2);
                         ("Al-Shehri", Sau, 1);
                         ("Alvarez", Arg, 1);
                         ("Chavez", Mex, 1);
                         ("Fernandez", Arg, 1);
                         ("Lewandowski", Pol, 1);
                         ("Mac Allister", Arg, 1);
                         ("Martin", Mex, 1);
                         ("Zielinski", Pol, 1)]))
      ] in
    let result = List.fold_left (&&) true (List.map snd l) in
    if result then (Printf.printf "The table_and_scorers test succeeds.\n"; [])
    else (Printf.printf "The table_and_scorers test fails.\n Check the corresponding line numbers in the list below.\n";
          (List.filter (fun (x,y) -> y=false) l) |> List.map fst)