open Game
open Util

let adjacent = [(1, 0); (0, 1); (-1, 0); (0, -1)]

(** [in_bounds g c r] is whether the stone at column [c] and row [r] is within 
    the bounds of the board. *)
let in_bounds game (col,row) = 
  let max_size = board_size game in
  col > 0 && col < max_size 
  &&
  row > 0 && row < max_size

let score game =
  failwith "unimplemented"

(** [is_empty game pos] is whether there is no stone current at position [pos]
    on the board in [game]. *)
let is_empty game pos = 
  not (List.mem pos (stones game Black) 
    || List.mem pos (stones game White))

(** [connected game pos] is the position of all stones of the same color as 
    the stone at [pos] that are adjacently-connected to [pos]. *)
let connected game pos =
  let stones =
    if List.mem pos (stones game Black) then stones game Black 
    else (stones game White) in 
  let stack = ref [] in
  let visited = ref [] in
  (** [find_same_adj pos] finds all stones of the same color as the one at [pos]
      that have not been visited or currently in [stack], and adds (by mutating)
      these new stones to the [stack]. *)
  let find_same_adj pos =
    let boundary = 
      List.map (fun a -> combine (+) pos a) adjacent |> 
      List.filter (fun spos -> 
        List.mem pos stones 
        && 
        not (List.mem pos !visited)
        &&
        not (List.mem pos !stack)) 
    in
    stack := !stack @ boundary
  in
  (** TODO *)
  let rec connected_r pos = 
    visited := !visited @ (find_same_adj pos); (* TODO: does not recurse *)
  in connected_r pos; !acc
  (* Get coordinate, check adjacent stones, make sure on recursive call not going back to original*)

let liberties game pos =
  let all_liberties = 
    List.map (fun c -> if is_empty game c then 1 else 0) (connected game pos) 
  in List.fold_left (fun acc v -> acc + v) 0 all_liberties

(** [n_stones] is the list containing the index of the move where [n] stones 
    were in play. *)
let n_stones game n =
  failwith "unimplemented"

let ko go (col,row) = 
  failwith "unimplemented"
