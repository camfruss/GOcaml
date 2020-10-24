(** Need to add prisoner list and game time. Anything that changes *)
type t = {
  player_turn : string;
  white_cur_stones : int;
  black_cur_stones : int;
  white_moves : (int * int * int) list;
  black_moves : (int * int * int) list
}

let init_state game =
  {
    player_turn = "b"; 
    white_cur_stones = 0;
    black_cur_stones = 0;
    white_moves = [];
    black_moves = []
  }

(** [positions lst] is the first and second value in each element of [lst]. 
    Given a stone color of a board, this is the column and row of each stone of 
    that color. *)
let positions lst = 
  let rec pos_tr acc = function
    | (r, c, m) :: t -> pos_tr ((r, c) :: acc) t
    | [] -> acc
  in pos_tr [] lst

let stones st color =
  if color = Game.White then positions st.white_moves else 
    positions st.black_moves

let is_empty st pos = 
  not (List.mem pos (stones st Black) 
       || List.mem pos (stones st White))

type result = Legal of t | Illegal

(** [generate_new_w_state] returns a new state after black has just played a 
     stone.

     Currently does not account for capturing
*)
let generate_new_w_state thruple_move st =
  {
    player_turn = "w"; 
    white_cur_stones = st.white_cur_stones; (* Change to account for capturing*)
    black_cur_stones = st.black_cur_stones + 1;
    white_moves = st.white_moves;
    black_moves = thruple_move :: st.black_moves
  }

(** [generate_new_b_state] returns a new state after white has just played a 
     stone.

     Currently does not account for capturing.
*)
let generate_new_b_state thruple_move st =
  {
    player_turn = "b"; 
    white_cur_stones = st.white_cur_stones + 1; 
    black_cur_stones = st.black_cur_stones; (* Change to account for capturing*)
    white_moves = thruple_move :: st.white_moves;
    black_moves = st.black_moves
  }

(** [generate_new_state] returns a new state after a player plays a stone. *)
let generate_new_state thruple_move player_turn st = 
  if player_turn = "b" then generate_new_w_state thruple_move st else 
    generate_new_b_state thruple_move st

(** [generate_new_move_thruple] returns the thruple version of the move 
     including the cur_stones

     Currently does not account for capturing *)
let generate_new_move_thruple move player_turn st = 
  let cur_stones = if player_turn = "b" then st.black_cur_stones else 
      st.white_cur_stones in
  (fst move, snd move, cur_stones + 1)

let play move game st = 
  let player_turn = st.player_turn in
  let new_move = generate_new_move_thruple move player_turn st in
  if is_empty st move then 
    Legal (generate_new_state new_move player_turn st) else 
    Illegal

let turn_start_text game st = 
  let p1_stone = String.make 1 (Game.get_p1_stone game) in
  if st.player_turn = p1_stone then "It is Player 1's turn. Make a move!" else
    "It is Player 2's turn. Make a move!"


