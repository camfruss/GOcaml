
let combine op (u1,u2) (v1,v2) =
  (op u1 v1, op u2 v2)

let string_of_string_list s =
  let rec str acc = function
    | [] -> acc
    | h :: [] -> acc ^ ", " ^ h
    | h :: t -> str (acc ^ ", " ^ h) t
  in "[" ^ (str "" s) ^ "]"  
  