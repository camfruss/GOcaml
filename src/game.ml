open Core.Out_channel
open Util
open Yojson.Basic.Util

(** TODO *)
type player = {
  id : string;
  prisoners : string list;
  stone : char;
  byoyomi : int;
  game_time : int;
}

(** TODO *)
type players = {
  p1 : player;
  p2 : player;
}

(** TODO *)
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

(** TODO *)
type board = {
  size : int;
  white : (string * int) list;
  black : (string * int) list
}

(** TODO *)
let to_board json =
  let rec player_stones acc = function
    | [] -> acc
    | h :: t ->
      let col = h |> member "col" |> to_string in
      let row = h |> member "row" |> to_string in
      let move = h |> member "row" |> to_int in
      player_stones ((col ^ row, move) :: acc) t
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

let to_config json = 
  let turn = 
    if json |> member "turn" |> to_string = "b" then 'b' else 'w' 
  in
  {
    byoyomi_period = json |> member "byoyomi_period" |> to_int;
    komi = json |> member "komi" |> to_float;
    turn = turn;
  }

(** TODO *)
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

(** TODO *)
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

(** *)
let from_players ps = 
  Printf.sprintf {|
    "players" : {
      %s,
      %s
    }
  |} (from_player ps.p1 "p1") (from_player ps.p2 "p2")

(** TODO *)
let from_move col row mov = 
  Printf.sprintf {|
    {      
      "col" : %d,
      "row" : %d,
      "move" : %d
    }
  |} col row mov

(** TODO *)
let from_board b = 
  Printf.sprintf {|
    "board" : {
      "size" : %d,
      "white" : [],
      "black" : []
    }
  |} b.size

(** TODO *)
let from_config c = 
  Printf.sprintf {|
    "config" : {
      "byoyomi_period" : %d,
      "komi" : %f,
      "turn" : %c
    }
  |} c.byoyomi_period c.komi c.turn

let to_json t out_file =
  let json_file = 
    Printf.sprintf {|
      %s,
      %s,
      %s
    |} (from_players t.players) (from_board t.board) (from_config t.config)
  in write_all out_file ~data:json_file

(** TODO *)
let stones t color =
  if color = "white" then t.board.white else t.board.black

(** TODO *)
let board_size t = 
  t.board.size
