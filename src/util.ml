
let combine_t op (u1,u2) (v1,v2) =
  (op u1 v1, op u2 v2)

let string_of_list cast lst =
  let rec str acc = function
    | [] -> acc
    | h :: [] -> 
      if acc = "" then cast h
      else acc ^ ", " ^ (cast h)
    | h :: t -> 
      if acc = "" then str (cast h) t
      else str (acc ^ ", " ^ cast h) t
  in "[" ^ (str "" lst) ^ "]" 

let max_triple3 lst = 
  List.fold_left 
    (fun acc (_,_,elt) -> if elt > acc then elt else acc) 0 lst 

let quartet_swap arr = 
  let rec swap acc = function
  | (v1, v2, v3, v4) :: t -> swap ((v2, v1, v4, v3) :: acc) t
  | [] -> Array.of_list acc
  in swap [] (Array.to_list arr)
