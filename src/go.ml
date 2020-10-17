open Game
open Util

let adjacent = [(1, 0); (0, 1); (-1, 0); (0, -1)]

(** [istone_pos pos] is the integer position of [pos] TODO
    Requires: [str] is a single capitalized alphabetic character followed by 
    integer in the interval [0,size). *)
let istone_pos pos =
  let col = Char.code (String.get pos 0) - 65 in
  let row = int_of_string (Str.string_after pos 1) 
  in (col, row)

(** [sstone_pos] is the string representation of [pos] *)
let sstone_pos (c,r) =
  let col = Char.chr (c + 65) |> Char.escaped in
  let row = string_of_int r
  in col ^ row

(** [in_bounds g c r] is whether the stone at column [c] and row [r] is within 
    the bounds of the board. *)
let in_bounds game (c, r) = 
  let max_size = board_size game in
  c > 0 && c < max_size 
  &&
  r > 0 && r < max_size

let score game =
  failwith "unimplemented"

(** [is_empty] is whether there is no stone current at position [pos]. *)
let is_empty game pos = 
  not (List.mem_assoc pos (stones game "black") 
    || List.mem_assoc pos (stones game "white"))

(** [connected go pos] is the position of all stones of the same color as [pos] 
    connected to [pos]. *)
let connected game spos =
  let bstones = stones game "black" in
  let stones = 
    if List.mem_assoc spos bstones then bstones else (stones game "white") in 
  (** TODO *)
  let acc = ref [] in
  let find_same_adj spos =
    let ipos = istone_pos spos 
    in
    List.map (fun a -> combine (+) ipos a) adjacent |> 
    List.map (fun ipos -> sstone_pos ipos) |>
    List.filter (fun spos -> List.mem_assoc spos stones && not (List.mem spos !acc))
  in
  (** TODO *)
  let rec connected_r spos = 
    acc := !acc @ (find_same_adj spos); (* TODO: does not recurse *)
  in connected_r spos; !acc
  (* Get coordinate, check adjacent stones, make sure on recursive call not going back to original*)


(*let adj_c = List.map (fun adj -> combine ( + ) adj c) adjacent in
    List.map (fun cr -> if List.mem (_, sstone_pos cr) op_stones then 0 else
    if List.mem (_, sstone_pos cr) your_stones then lib_r cr else 1) (adj_c)
      let coord = istone_pos pos in
  
    *)

let liberties game pos =
  let all_liberties = 
    List.map (fun c -> if is_empty game c then 1 else 0) (connected game pos) 
  in List.fold_left (fun acc v -> acc + v) 0 all_liberties

(** [n_stones] is the list containing the index of the move where [n] stones 
    were in play. *)
let n_stones game n =
  failwith "unimplemented"

let ko go str = 
  failwith "unimplemented"
