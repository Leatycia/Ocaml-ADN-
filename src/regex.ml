open Regex_base

let rec repeat n l =
  if n<=0 then []
  else l @ repeat (n-1) l

let rec expr_repeat n e =
  if n<=0 then Eps
  else Concat (e,expr_repeat (n-1) e)

let rec is_empty e =
   match e with
  | Eps -> true
  |Star (Eps)->true
  |Concat(Eps,Eps)->true
  | Concat (left, right) -> is_empty left && is_empty right
  | Star x -> is_empty x 
  | Base _ -> false
  | Joker -> false  
  | Alt (left, right) -> is_empty left && is_empty right

let rec null e =
  if is_empty e then true
  else
    match e with
    | Eps -> true
    | Star exp -> true 
    | Concat (a, b) -> null a && null b
    | Base _ -> false
    | Alt (left, right) -> null left || null right
    | Joker -> false

let rec aux a b=
  if List.mem b a then false else
    match b with
    | Eps -> true
    | Base _ -> true
    | Joker -> true
    | Concat (x1,x2) -> aux a x1 && aux a x2
    | Alt (x1,x2) -> aux a x1 && aux a x2
    | Star x -> is_empty x

let rec is_finite e =
  aux [] e
  

let product l1 l2 =
  let rec aux l1 l2 acc =
    match l1 with
    | [] -> acc
    | head_a :: tail_a ->
      let product_with_head = List.map (fun x -> head_a @ x) l2 in
      let updated_acc = acc @ product_with_head in
      aux tail_a l2 updated_acc
  in
  aux l1 l2 []

let rec enumerate alphabet e =
  if not (is_finite e) then None
  else if null e then Some [[]] 
  else
    let rec enumerate_helper acc current_list = function
      | [] ->
        if current_list = [] || List.length current_list > 1 then acc
        else List.rev (current_list :: acc)
      | x :: xs ->
        match e with
        | Joker -> enumerate_helper (List.rev (x :: current_list) :: acc) [] xs
        | Base c when x = c -> enumerate_helper (List.rev (x :: current_list) :: acc) [] xs
        | Base _ -> enumerate_helper acc (current_list @ [x]) xs
        | _ -> enumerate_helper (List.rev (x :: current_list) :: acc) [] xs
    in
    match e with
    | Joker -> Some (enumerate_helper [] [] alphabet)
    | Base _ -> Some (enumerate_helper [] [] alphabet)
    | Concat (e1, e2) ->
      (match (enumerate alphabet e1), (enumerate alphabet e2) with
      | Some l1, Some l2 -> Some (product l1 l2)
      | _, _ -> None)
    | Alt (e1, e2) ->
      (match (enumerate alphabet e1), (enumerate alphabet e2) with
      | Some l1, Some l2 -> Some (union_sorted l1 l2)
      | _, _ -> None)
    | _ -> None 

let rec alphabet_expr e =
  match e with 
  | Eps -> []
  | Base x -> [x]
  | Joker -> []
  | Concat (x1,x2) -> union_sorted (alphabet_expr x1) (alphabet_expr x2)
  | Alt (x1,x2) -> union_sorted (alphabet_expr x1) (alphabet_expr x2)
  | Star x -> sort_uniq (alphabet_expr x)


type answer =
  Infinite | Accept | Reject

(* Création d'une liste sans la trié avec doublons *)
let rec creer_list e=
  match e with 
  | Eps->[]
  | Base x -> [x]
  | Joker -> []
  | Concat (x1,x2) -> creer_list x1 @ creer_list x2
  | Alt (x1,x2) -> creer_list x1 @ creer_list x2
  | Star x -> creer_list x
  

(* Vérifie que chaque éléments de liste1 est dans liste2 *)
let rec sont_egales liste1 liste2 =
  match liste1 with
  | [] -> true
  | hd :: tl ->
    if not (List.mem hd liste2) then
      false
    else
      sont_egales tl liste2
  

let rec accept_partial e w = 
  match e with 
  | Eps -> if (List.mem [] [w]) = true then Accept else Reject
  | Base a -> if (List.mem [a] [w]) = true then Accept else Reject
  | Joker -> Accept
  | Concat _ -> 
    let l=creer_list e in 
    let check=sont_egales w l in 
    let finite=is_finite e in 
    if finite && w=[] then Reject else
    if finite && l = w || (List.length w!=List.length l && check && List.hd w=List.hd l) || 
    (List.length w!=List.length l && check  && List.hd w=List.hd (List.tl l)) 
     then Accept else Reject
  | Alt _ -> 
    let l=alphabet_expr e in
    let check=sont_egales w l in
    if check || w=[]  then Accept else Reject
  | Star a -> Infinite