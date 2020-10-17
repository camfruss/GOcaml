open Util
open Yojson.Basic.Util

type stone = Black | White

(** [player] is the type representing a single player in a go game. *)
type player = {
  id : string;
  prisoners : string list;
  stone : char;
  byoyomi : int;
  game_time : int;
}

(** [players] is the type representing a set of 2 players in a go game. *)
type players = {
  p1 : player;
  p2 : player;
}

(** [to_player json] is the player record associated by the valid json.  *)
let to_player json = 
  let stone = 
    if json |> member "stone" |> to_string = "b" then 'b' else 'w' in
  let prisoners = json 
    |> member "prisoners" 
    |> to_list 
    |> List.map (fun elt -> elt |> to_string) in
  {
    byoyomi = json |> member "byoyomi" |> to_int;
    game_time = json |> member "game_time" |> to_int;
    id = json |> member "id" |> to_string;
    prisoners = prisoners;
    stone = stone;
  }

(** [board] is the type presenting a single go board. *)
type board = {
  (** [size] is the nxn dimensions of a go board. *)
  size : int;
  (** [white] and [black] represent the column, row, and move number of each 
      stone, respectively. The column number is the index of the character of 
      the column in the alphabet. 
      Example:
      - 1, 1, 1 would be the first stone of some color placed at B1 .*)
  white : (int * int * int) list;
  black : (int * int * int) list
}

(** [to_board json] is the board record represented by the valid json. *)
let to_board json =
  let rec player_stones acc = function
    | [] -> acc
    | h :: t ->
      let col = h |> member "col" |> to_int in
      let row = h |> member "row" |> to_int in
      let move = h |> member "move" |> to_int in
      player_stones ((col,row,move) :: acc) t
  in
  {
    size = json |> member "size" |> to_int;
    white = json |> member "white" |> to_list |> player_stones [];
    black = json |> member "white" |> to_list |> player_stones [];
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

(** [to_config j] is the [config] record corresponding to [json] *)
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
  Printf.sprintf {|
    "%s" : {
      "byoyomi" : %d,
      "game_time" : %d,
      "id" : "%s",
      "prisoners" : %s,
      "stone" : "%c"
    }
  |} name p.byoyomi p.game_time p.id (string_of_string_list p.prisoners) p.stone

(** [from_players ps] is the json representation of a [players] record. *)
let from_players ps = 
  Printf.sprintf {|
    "players" : {
      %s,
      %s
    }
  |} (from_player ps.p1 "p1") (from_player ps.p2 "p2")

(** [from_move m] is the json representation of a single move as specified by 
    the column, row, and move number of a given stone. *)
let from_move col row mov = 
  Printf.sprintf {|
    {      
      "col" : %d,
      "row" : %d,
      "move" : %d
    }
  |} col row mov

(** [from_board b] is the json representation of a [board] record [b]. *)
let from_board b = 
  Printf.sprintf {|
    "board" : {
      "size" : %d,
      "white" : [],
      "black" : []
    }
  |} b.size

(** [from_config c] is the json representation of a [config] record [c]. *)
let from_config c = 
  Printf.sprintf {|
    "config" : {
      "byoyomi_period" : %d,
      "komi" : %f,
      "turn" : %c
    }
  |} c.byoyomi_period c.komi c.turn

let to_json t out_file =
  let content = 
    Printf.sprintf {|
      %s,
      %s,
      %s
    |} (from_players t.players) (from_board t.board) (from_config t.config)
  in 
  let oc = open_out out_file in
  Printf.fprintf oc "%s\n" content;
  close_out oc

(** [positions lst] is the first and second value in each element of [lst]. 
    Given a stone color of a board, this is the column and row of each stone. *)
let positions lst = 
  let rec pos_tr acc = function
    | (r, c, m) :: t -> pos_tr ((r, c) :: acc) t
    | [] -> acc
  in pos_tr [] lst

let stones t color =
  if color = White then positions t.board.white else positions t.board.black

let board_size t = 
  t.board.size
