(* Question 1. *)

let rec pairlists (l1, l2) =
  let l = List.fold_left2 (fun acc element1 element2 -> (element1, element2) :: acc) [] l1 l2 in
  List.rev l
;;

let wmean weights data =
  let pairs = pairlists (weights, data) in
  let weightedData = List.map (fun (weight, data) -> weight *. data) pairs in
  let weightedSum = sumlist weightedData in
  let sumOfWeights = sumlist weights in
  weightedSum /. sumOfWeights
;;

(* Question 2. *)

let rec memberof (n, l) =
  match l with
  | [] -> false
  | h :: t ->
    if h = n then true
    else memberof (n, t)
;;

let rec remove (item, lst) =
  match lst with
  | [] -> []
  | h :: t ->
    if h = item then remove (item, t)
    else h :: remove (item, t)
;;

(* Question 3. *)

let find_max l =
  let rec find_max_aux cur_max lst =
    match lst with
    | [] -> cur_max
    | h :: t ->
      if h > cur_max then find_max_aux h t
      else find_max_aux cur_max t
  in
  find_max_aux (List.hd l) l
;;

(* Question 4. *)

let rec selsort l =
match l with
| [] -> []
| _ -> let m = find_max l in m :: selsort (remove (m, l))
;;
