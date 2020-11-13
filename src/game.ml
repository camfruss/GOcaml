open Util
open Yojson.Basic.Util

exception KoException

exception SelfCaptureException

exception StoneAlreadyExistsException (* TODO: Remove this one in command.ml *)

exception TimeExpiredException

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
  let prisoners = 
    json 
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
      - 1, 1, 1 would be the stone placed B2, which becomes the only stone on 
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
  let oc = open_out out_file in (** TODO: if doesn't end in .json,InvalidFile *)
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
  | Black -> positions t.board.black
  | White -> positions t.board.white

let adjacent = [(1, 0); (0, 1); (-1, 0); (0, -1)]

let in_bounds t (col,row) = 
  let max_size = t.board.size in
  col >= 0 && col < max_size 
  &&
  row >= 0 && row < max_size

let bounds t = 
  t.board.size

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
  else if List.mem pos (stones t White) then stones t White 
  else []

(** [stone_color t pos] is tjhe color of the stone at position [pos] in game 
    [t]. *)
let stone_color t pos = 
  if List.mem pos (stones t Black) then Some Black 
  else if List.mem pos (stones t White) then Some White 
  else None

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
    stack := !stack @ boundary
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

let ko t (c,r) = false
(* failwith "unimplemented" *) (** TODO *)

let turn t =
  if t.config.turn = 'b' then Black else White

(** TOOD *)
let cur_player t = 
  match turn t with
  | Black -> t.players.p1
  | White -> t.players.p2

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
  match n_moves (turn t) with 
  | (_,_,n) -> n - n_prisoners

let last_stone t = 
  let s = match turn t with
    | White -> t.board.black 
    | Black -> t.board.white
  in 
  match max_triple3 s with
  | (c,r,_) -> (c,r)

(** [deduct_time t time] is the game with the proper time parameters after 
    the player who just went spent [time] seconds on their move. *)
let deduct_time t time = 
  let p = cur_player t in
  let t_new = 
    if time <= p.game_time then (p.byoyomi, p.game_time - time)
    else let periods_used = (time - p.game_time) / t.config.byoyomi_period in
      if p.byoyomi > periods_used then (p.byoyomi - periods_used, 0)
      else raise TimeExpiredException
  in t_new

(** [remove_stones t s] removes all the stones in [s] from the board in [t]. *)
let remove_stones t s =
  let remove_cr t (c,r) = 
    let b = match turn t with
      | Black -> t.board.black 
      | White -> t.board.white
    in
    List.filter (fun (c', r' , _) -> not (c' = c && r' = r)) b
  in
  let rec remove_stone t = function
    | (c, r) :: tail -> begin
        let board' = match turn t with
          | White -> {t.board with white = remove_cr t (c,r)}
          | Black -> {t.board with black = remove_cr t (c,r)}
        in
        let t' = {t with board = board'} 
        in remove_stone t' tail
      end
    | [] -> t
  in remove_stone t s


(** [remove_prisoners t pos] checks the board in [t] and determines if any 
    stones were captured from the stone placed at position [pos]. *)
let remove_prisoners t pos =
  let adj = List.filter 
      (fun p -> 
         in_bounds t p 
         && 
         stone_color t p = Some (turn t)
      ) (c_adjacent pos) in
  let groups = List.map (fun a -> (liberties t a, group t a)) adj in
  List.fold_left 
    (fun acc (libs, stones) -> 
       if libs = 0 then remove_stones t stones else acc
    ) t groups

let self_sacrifice t pos = 
  let adj = List.filter (fun p -> in_bounds t p) (c_adjacent pos) in
  let max_adj_liberties = 
    match list_max (List.map (fun p -> liberties t p) adj) with
    | Some v -> v
    | None -> 0
  in
  if List.fold_left (fun acc pos -> acc + liberties t pos) 0 (group t pos) = 0 
     && 
     max_adj_liberties > 0
  then raise SelfCaptureException 
  else ()

let handicap t lst = 
  let n, placements = List.fold_left (fun (n, acc) (c,r) -> (n + 1, (c, r, n) :: acc)) (1, []) lst in
  let board' = {t.board with black = placements} in
  let config' = {t.config with turn = 'w'} 
  in {t with board = board'; config = config'}

let new_player t time =
  let (byoyomi', game_time') = deduct_time t time in
  let p = cur_player t in { 
    p with byoyomi = byoyomi'; 
           game_time = game_time';
  }

let new_players t time = 
  let p = new_player t time in
  match turn t with
  | Black -> {t.players with p1 = p}
  | White -> {t.players with p2 = p}

(** [new_board t m] is the updated board for [t] after move [m]. *)
let new_board t (c,r) = 
  if not (is_empty t (c,r)) then raise StoneAlreadyExistsException
  else if ko t (c,r) then raise KoException 
  else 
    let move' = (c, r, 1 + n_stones t) in
    match (turn t) with
    | Black -> {t.board with black = move' :: t.board.black}
    | White -> {t.board with white = move' :: t.board.white}

let new_config t = 
  let turn' = if (turn t) = Black then 'w' else 'b' in
  {t.config with turn = turn'}

let step t move time = 
  let players' = new_players t time in
  let board' = new_board t move in
  let config' = new_config t in
  let t' = {
    players = players';
    board = board';
    config = config'
  } in
  let t'' = remove_prisoners t' move in
  self_sacrifice t'' move; t''

(** [full_board t] creates an nxn matrix consisting of dots to represent an 
    empty Go board of size n. *)
let full_board t = 
  let dim = t.board.size in 
  let grid = Array.make_matrix dim dim "⋅" in
  let rec add_stones rep = function 
    | (c, r, _) :: t -> grid.(r).(c) <- rep; add_stones rep t
    | [] -> ()
  in add_stones "W" t.board.white; add_stones "B" t.board.black; grid

let string_of_string_array arr =  
  String.concat " " (Array.to_list arr)

let string_of_string_string_array arr =
  let lst = Array.map string_of_string_array arr |> Array.to_list in
  String.concat "\n" lst

let string_of_board t =
  string_of_string_string_array (full_board t)

(* Scoring  *)
type territory = WhiteT | BlackT | Neutral | WhiteS | BlackS | Empty

let score t =
  let dim = t.board.size in
  let grid = Array.make_matrix dim dim Empty in
  (** [populate_grid s t] marks all the stones in [s] to [t]. *)
  let populate_grid s territory = 
    List.iter (fun (c, r) -> grid.(r).(c) <- territory) s
  in
  populate_grid (stones t Black) BlackS; populate_grid (stones t White) WhiteS;
  let explore pos =
    let stack = ref [pos] in
    let visited = ref [] in
    let boundary = ref [] in
    let traverse pos = 
      let adj = 
        List.filter (fun (c, r) -> in_bounds t (c, r)) (c_adjacent pos) in
      let stackable = 
        List.filter 
          (fun (c, r) -> 
             (grid.(r).(c) != BlackS && grid.(r).(c) != WhiteS)
             && 
             not (List.mem (c, r) !visited)
          ) adj in
      let unstackable = 
        List.filter 
          (fun (c, r) -> 
             (grid.(r).(c) = BlackS || grid.(r).(c) = WhiteS)
             && 
             not (List.mem (c, r) !visited)
          ) adj in
      boundary := !boundary @ unstackable;
      visited := pos :: !visited;
      stack := !stack @ stackable;
    in 
    while !stack != [] do
      let h = List.hd !stack in (* Note: safe, as stack is not empty. *)
      stack := List.tl !stack;
      traverse h; 
    done; (!visited, !boundary)
  in
  let has_stone lst territory = 
    let filtered = List.filter (fun (c, r) -> grid.(r).(c) = territory) lst 
    in List.length filtered > 0
  in
  let mark lst territory = 
    List.iter (fun (c, r) -> grid.(r).(c) <- territory) lst
  in
  for i = 0 to Array.length grid - 1 do
    for j = 0 to Array.length grid.(i) - 1 do 
      let visited, boundary = explore (j, i) in
      let has_black = has_stone boundary BlackS in
      let has_white = has_stone boundary WhiteS in
      let has_neutral = has_stone boundary Neutral in
      if has_neutral then mark visited Neutral
      else if has_black && not has_white then mark visited BlackT
      else if not has_black && has_white then mark visited WhiteT
    done
  done;
  let flattened = 
    Array.map (fun x -> Array.to_list x) grid |> Array.to_list |> List.flatten 
  in
  let black = List.filter (fun e -> e = BlackT) flattened |> List.length in
  let white = List.filter (fun e -> e = WhiteT) flattened |> List.length in
  let b_prisoners = List.length t.players.p1.prisoners in
  let w_prisoners = List.length t.players.p2.prisoners in
  let komi = t.config.komi 
  in (komi +. float_of_int (white + w_prisoners), 
      float_of_int (black + b_prisoners))
