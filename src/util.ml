
let combine_t op (u1,u2) (v1,v2) =
  (op u1 v1, op u2 v2)

let string_of_int_list lst =
  let rec str acc = function
    | [] -> acc
    | h :: [] -> 
      if acc = "" then string_of_int h
      else acc ^ ", " ^ (string_of_int h)
    | h :: t -> str (acc ^ ", " ^ string_of_int h) t
  in "[" ^ (str "" lst) ^ "]"  

let max_triple3 lst = 
  List.fold_left 
    (fun acc (_,_,elt) -> if elt > acc then elt else acc) Int.min_int lst 
