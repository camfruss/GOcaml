(** Need to add prisoner list and game time. Anything that changes *)
type t = {
  player_turn : string;
  white_cur_stones : int;
  black_cur_stones : int;
  white_moves : (int * int * int) list;
  black_moves : (int * int * int) list
}

let init_state game =
  (* let starting_id = Adventure.start_room adv in
     {
     state_id = starting_id;
     visited_rooms = [starting_id]
     } *)
  {
    player_turn = "b"; 
    white_cur_stones = 0;
    black_cur_stones = 0;
    white_moves = [];
    black_moves = []
  }

type result = Legal of t | Illegal

(** [shorten] returns just the (col, row) of a players [thruple_move]. Each 
    thruple_move is given by (col, row, cur_stones).
*)
let shorten (thruple_move : (int * int * int)) = 
  match thruple_move with
  | (x,y,_) -> (x,y)

let generate_new_state move cur_stones move_lst game st = 
  failwith "unimmplemented"



let play move game st = 
  let move_lst  = if st.player_turn = "b" then st.black_moves else st.white_moves in
  let move_lst_short = List.map shorten move_lst in
  let new_move = 
    if List.mem move move_lst_short then Legal else Illegal
