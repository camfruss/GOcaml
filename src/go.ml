open Game
open Util

let adjacent = [(1, 0); (0, 1); (-1, 0); (0, -1)]

(** [in_bounds g c r] is whether the stone at column [c] and row [r] is within 
    the bounds of the board. *)
let in_bounds game (col,row) = 
  let max_size = board_size game in
  col >= 0 && col < max_size 
  &&
  row >= 0 && row < max_size

let score game =
  failwith "unimplemented"

(** [is_empty game pos] is whether there is no stone current at position [pos]
    on the board in [game]. *)
let is_empty game pos = 
  if (in_bounds game pos) then 
    not (List.mem pos (stones game Black) 
         || List.mem pos (stones game White))
  else false

(** [c_adjacent pos] is the coordinates of all the positions adjacent to 
    [pos]. *)
let c_adjacent pos =
  List.map (fun a -> combine (+) pos a) adjacent

(** [group game pos] is the group of stones of the same color as the stone at 
    [pos] that are adjacently-connected to [pos]. *)
let group game pos =
  let stones =
    if List.mem pos (stones game Black) then stones game Black 
    else (stones game White) in 
  let stack = ref [pos] in
  let visited = ref [] in
  (** [find_same_adj pos] finds all stones of the same color as the one at [pos]
      that have not been visited or currently in [stack], and adds (by mutating)
      these new stones to the [stack]. *)
  let find_same_adj pos =
    let boundary = 
      c_adjacent pos |> 
      List.filter (fun pos -> 
          List.mem pos stones 
          && 
          not (List.mem pos !visited)
          &&
          not (List.mem pos !stack)) 
    in
    visited := pos :: !visited;
    stack := !stack @ boundary; ()
  in
  (** [connected_r pos] is the group of stones connected to [pos]. *)
  let rec connected_r pos =
    while !stack != [] do
      find_same_adj (List.hd !stack); (* Safe, as stack is not empty. *)
      stack := List.tl !stack;
    done
  in connected_r pos; !visited

let liberties game pos =
  let connected = group game pos in
  let all_adjacent = 
    List.map (fun s -> c_adjacent s) connected 
    |> List.flatten 
    |> List.sort_uniq compare in
  let all_liberties = 
    List.map (fun c -> if is_empty game c then 1 else 0) all_adjacent 
  in List.fold_left (fun acc v -> acc + v) 0 all_liberties

(** [n_stones] is the list containing the index of the move where [n] stones 
    were in play. *)
let n_stones game n =
  failwith "unimplemented"

let ko go (col,row) = 
  failwith "unimplemented"
