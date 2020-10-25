open Util
open Yojson.Basic.Util

exception KoException

exception SelfCaptureException

type stone = Black | White

(** [player] is the type representing a single player in a go game. *)
type player = {
  id : string;
  prisoners : int list;
  byoyomi : int;
  game_time : int;
}

(** [players] is the type representing a set of 2 players in a go game. *)
type players = {
  p1 : player;
  p2 : player;
}

(** [to_player json] is the player record represented by a valid player.  *)
let to_player json = 
  let prisoners = json 
    |> member "prisoners" 
    |> to_list 
    |> List.map (fun elt -> elt |> to_int) in
  {
    byoyomi = json |> member "byoyomi" |> to_int;
    game_time = json |> member "game_time" |> to_int;
    id = json |> member "id" |> to_string;
    prisoners = prisoners;
  }

(** [board] is the type presenting a single go board. *)
type board = {
  (** [size] is the nxn dimensions of a go board. *)
  size : int;
  (** [white] and [black] represent the column, row, and cur_stones in play 
      after the move, respectively. The column number is the index of the 
      character of the column in the alphabet. 
      Example:
      - 1, 1, 1 would be the stone placed B1, which becomes the only stone on 
        the board. *)
  white : (int * int * int) list;
  black : (int * int * int) list
}

(** [to_board json] is the board record represented by the valid json board. *)
let to_board json =
  let rec player_stones acc = function
    | [] -> List.rev acc
    | h :: t ->
      let col = h |> member "col" |> to_int in
      let row = h |> member "row" |> to_int in
      let cur_stones = h |> member "cur_stones" |> to_int in
      player_stones ((col,row,cur_stones) :: acc) t
  in
  {
    size = json |> member "size" |> to_int;
    white = json |> member "white" |> to_list |> player_stones [];
    black = json |> member "black" |> to_list |> player_stones [];
  }

type config = {
  (** [byoyomi_period] is the length in seconds of each byo-yomi period. *)
  byoyomi_period : int;
  (** [komi] is the compensation awarded to the player who goes second, who is 
      placed at a measurable disadvantage. *)
  komi : float;
  (** [turn] is the player whose turn is currently is. *)
  turn : char;
}

(** [to_config j] is the [config] record corresponding to the config [json]. *)
let to_config json = 
  let turn = 
    if json |> member "turn" |> to_string = "b" then 'b' else 'w' 
  in
  {
    byoyomi_period = json |> member "byoyomi_period" |> to_int;
    komi = json |> member "komi" |> to_float;
    turn = turn;
  }

type t = {
  players : players;
  board : board;
  config : config;
}

let from_json json =
  let players = json |> member "players" in
  let p1 = players |> member "p1" |> to_player in
  let p2 = players |> member "p2" |> to_player in
  let board = json |> member "board" |> to_board in
  let config = json |> member "config" |> to_config in
  {
    players = {
      p1 = p1;
      p2 = p2;
    };
    board = board;
    config = config;
  }

(** [from_player p name] is the json representation of a [player] record with 
    key [name]. *)
let from_player p name =
  Printf.sprintf 
    {|"%s" : {
        "byoyomi" : %d,
        "game_time" : %d,
        "id" : "%s",
        "prisoners" : %s
    }|} name p.byoyomi p.game_time p.id 
       (string_of_list string_of_int p.prisoners)

(** [from_players ps] is the json representation of a [players] record. *)
let from_players ps = 
  Printf.sprintf 
    {|"players" : {
      %s,
      %s
    }|} (from_player ps.p1 "p1") (from_player ps.p2 "p2")

(** [from_move m] is the json representation of a single move as specified by 
    the column, row, and move number of a given stone. *)
let from_move col row mov = 
  Printf.sprintf 
    {|{      
      "col" : %d,
      "row" : %d,
      "cur_stones" : %d
    }|} col row mov

(** [from_board b] is the json representation of a [board] record [b]. *)
let from_board b = 
  let moves lst = 
    List.map (fun (c,r,m) -> from_move c r m) lst 
    |> string_of_list (fun id -> id)
  in
  Printf.sprintf 
    {|"board" : {
        "size" : %d,
        "white" : %s,
        "black" : %s
    }|} b.size (moves b.white) (moves b.black)

(** [from_config c] is the json representation of a [config] record [c]. *)
let from_config c = 
  Printf.sprintf 
    {|"config" : {
        "byoyomi_period" : %d,
        "komi" : %f,
        "turn" : "%c"
    }|} c.byoyomi_period c.komi c.turn

let to_json t out_file =
  let content = 
    Printf.sprintf 
      {|{ 
        %s,
        %s,
        %s
      }|} (from_players t.players) (from_board t.board) (from_config t.config)
  in 
  let oc = open_out out_file in
  Printf.fprintf oc "%s\n" content;
  close_out oc

(** [positions lst] is the first and second value in each element of [lst]. 
    Given a stone color of a board, this is the column and row of each stone of 
    that color. *)
let positions lst = 
  let rec pos_tr acc = function
    | (r, c, m) :: t -> pos_tr ((r, c) :: acc) t
    | [] -> acc
  in pos_tr [] lst

let stones t = function
  | White -> positions t.board.white
  | Black -> positions t.board.black

let adjacent = [(1, 0); (0, 1); (-1, 0); (0, -1)]

let in_bounds t (col,row) = 
  let max_size = t.board.size in
  col >= 0 && col < max_size 
  &&
  row >= 0 && row < max_size

let is_empty t pos = 
  if in_bounds t pos then 
    not (List.mem pos (stones t Black) 
      || List.mem pos (stones t White))
  else false

(** [c_adjacent pos] are the coordinates of all the positions adjacent to 
    [pos]. *)
let c_adjacent pos =
  List.map (fun a -> combine_t (+) pos a) adjacent

(** [pos_stones t pos] is the list of stones in [t] that are the same color as 
    the stone at [pos]. *)
let pos_stones t pos = 
  if List.mem pos (stones t Black) then stones t Black 
  else (stones t White)

(** [group t pos] is the group of stones of the same color as the stone at 
    [pos] that are adjacently-connected to [pos]. *)
let group t pos =
  let stones = pos_stones t pos in
  let stack = ref [pos] in
  let visited = ref [] in
  (** [find_same_adj pos] finds all stones of the same color as the one at [pos]
      that have not been visited or currently in [stack], and adds (by mutating)
      these new stones to the [stack]. *)
  let find_same_adj pos =
    let boundary = 
      c_adjacent pos |> 
      List.filter 
        (fun pos -> 
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
      find_same_adj (List.hd !stack); (* Note: safe, as stack is not empty. *)
      stack := List.tl !stack;
    done
  in connected_r pos; !visited

let liberties t pos =
  let connected = group t pos in
  let all_adjacent = 
    List.map (fun s -> c_adjacent s) connected 
    |> List.flatten 
    |> List.sort_uniq compare in
  let all_liberties = 
    List.map (fun c -> if is_empty t c then 1 else 0) all_adjacent 
  in List.fold_left (fun acc v -> acc + v) 0 all_liberties

let score t =
  failwith "unimplemented"

let ko go (col,row) = 
  failwith "unimplemented"

let turn t =
  if t.config.turn = 'b' then Black else White

let names t = 
  (t.players.p1.id, t.players.p2.id)

(** [n_stones] is the number of stones currently on the board in [t]. *)
let n_stones t =
  let p1,p2 = t.players.p1.prisoners, t.players.p2.prisoners in
  let n_prisoners = List.length p1 + List.length p2 in
  let n_moves = function
    | Black -> max_triple3 t.board.white
    | White -> max_triple3 t.board.black
  in 
  n_moves (turn t) - n_prisoners

(** [deduct_time t time] is the game with the proper time parameters after 
    the player who just went spent [time] seconds on their move. *)
let deduct_time t time = 
  failwith "unimplemented"

(** [find_prisoners t] checks the board in [t] and determines if any stones were
    captured. 
    Raises: [SelfCaptureException] if the player who just moved is captured by 
    their opponent (i.e. self-capture). The player should be prompted to enter a
    new move. *)
let find_prisoners t =
  failwith "unimplemented"

let new_players t = 
  failwith "unimplemented"

(** [new_board t m] is the updated board for [t] after move [m]. *)
let new_board t (c,r) = 
  let move' = (c, r, 1 + n_stones t) in
  match (turn t) with
  | Black -> {t.board with black = move' :: t.board.black}
  | White -> {t.board with white = move' :: t.board.white}

let new_config t = 
  let turn' = if (turn t) = Black then 'w' else 'b' in
  {t.config with turn = turn'}

let step t move time = 
  let board' = new_board t move in
  let config' = new_config t in
  {
    players = t.players;
    board = board';
    config = config'
  }
  