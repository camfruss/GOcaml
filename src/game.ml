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

(** [new_config t] is the update configuration for [t]. Updates which turn it is
*)
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

(** [new_players t] is the updated players for [t]. Updates the prisoners list 
     after a pass. *)
let new_players t = 
  let cur_stones = n_stones t in
  if turn t  = Black
  then let prisoners' = t.players.p2.prisoners @ [cur_stones] in 
    {t.players with p2 = {t.players.p2 with prisoners = prisoners'}}
  else let prisoners' = t.players.p1.prisoners @ [cur_stones] in 
    {t.players with p1 = {t.players.p1 with prisoners = prisoners'}}

let pass_update t time = 
  let players' = new_players t in
  let config' = new_config t in 
  {
    players = players';
    board = t.board;
    config = config'
  }

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

(**Scoring implementation *)

let set_color (col,row) stone grid = 
  grid.(col).(row) <- stone 

let grid_dim grid = Array.length grid 

(**[floodfill n b s] fills [board] with color [stone] of all other empty spaces 
    that are reached by [(col,row)].
    Requires: blank and stone cannot be of same color *)
let rec floodfill grid (col,row) blank stone = 
  let dim =  grid_dim grid in 
  let valid_pos (c,r) =
    c >= 0 && c < dim
    &&
    r >= 0 && r < dim
  in 
  if valid_pos (col,row) then  
    let node_color = grid.(col).(row) in 
    if node_color <> blank then ()
    else begin 
      set_color (col,row) stone grid;
      floodfill grid (col - 1, row) blank stone;
      floodfill grid (col + 1, row) blank stone;
      floodfill grid (col, row - 1) blank stone;
      floodfill grid (col, row + 1) blank stone;
    end 

(**[territory_finder g p c d] is true if [pos] is within [color]'s territory
    or neutral territory, false otherwise. Checks in order of n e s w *)
let rec territory_finder grid (col,row) color og_pos dir = 
  let grid_dim = Array.length grid in 
  let pos_color = grid.(col).(row) in 
  match pos_color with 
  | "W" -> if color = "W" then true 
    else territory_finder grid og_pos color og_pos (dir + 1)
  | "B" -> if color = "B" then true 
    else territory_finder grid og_pos color og_pos (dir + 1) 
  | "⋅" -> begin 
      match dir with 
      | 0 -> if col -1 >=0
        then territory_finder grid (col - 1, row) color og_pos 0 
        else territory_finder grid og_pos color og_pos 1 
      | 1 -> if row +1 < grid_dim
        then territory_finder grid (col, row + 1) color og_pos 1 
        else territory_finder grid og_pos color og_pos 2 
      | 2 -> if col +1 < grid_dim
        then territory_finder grid (col + 1, row) color og_pos 2 
        else territory_finder grid og_pos color og_pos 3 
      | 3 -> if row -1 >=0
        then territory_finder grid (col, row -1) color og_pos 3 
        else false
      | _ -> false 
    end 
  | _ ->  failwith "floodfill failed"

(**[fill t g c f (col,r)] is a nxn matrix with [color]'s territory and 
    neutral territory filled with [f]*)
let rec fill grid c f (col,row) = 
  let dim = Array.length grid in 
  if row = dim then fill grid c f (col +1,0)
  else if col = dim then grid 
  else begin
    let color = grid.(col).(row) in 
    match color with 
    | "⋅" -> begin
        if territory_finder grid (col,row) c (col,row) 0 then 
          (* let update_grid =  floodfill t grid (col,row) "⋅" f in  *)
          let x = 
            floodfill grid (col,row) "⋅" f;
            (fill  grid c f (col,row + 1)) in 
          x
        else  fill  grid c f (col,row + 1)
      end 
    | _ -> fill  grid c f (col,row + 1)
  end 

(**[score_helper w_g b_g w_s b_s (c,r)] calculates the score of the board*)
let rec score_helper w_grid b_grid w_score b_score (col,row)=
  let max_dim = Array.length w_grid in 
  if row = max_dim 
  then score_helper w_grid b_grid w_score b_score (col+1,0)
  else if col = max_dim then (b_score, w_score) 
  else
    let w_pos = w_grid.(col).(row) in 
    let b_pos = b_grid.(col).(row) in 
    match w_pos with 
    | "w" -> if b_pos = "⋅" 
      then score_helper w_grid b_grid (w_score +. 1.) b_score (col,row +1 )
      else 
        score_helper w_grid b_grid w_score b_score (col,row +1 )
    | "⋅" -> if b_pos = "b" 
      then score_helper w_grid b_grid w_score (b_score +. 1.) (col,row +1 )
      else 
        score_helper w_grid b_grid w_score b_score (col,row +1 )
    | _ -> score_helper w_grid b_grid w_score b_score (col,row +1 )

(**[create_grid t c f] is a board representation with [color]'s and netural 
    territory filled with [filler] *)
let create_grid t color filler= 
  let grid = full_board t in 
  fill grid color filler (0,0)

let comparer w_grid b_grid grid (c,r) = 
  let w_pos = w_grid.(c).(r) in 
  let b_pos = b_grid.(c).(r) in 
  match w_pos with 
  | "w" -> if b_pos = "⋅" then grid.(c).(r) <- "w"
  | "⋅" -> if b_pos = "b" then grid.(c).(r) <- "b"
  | _ -> ()

let rec m_t_helper  w_grid b_grid grid (c,r) = 
  let dim = grid_dim w_grid in 
  if r = dim then m_t_helper  w_grid b_grid grid (c +1,0)
  else if c = dim then grid 
  else 
    let x = comparer  w_grid b_grid grid (c,r);
      m_t_helper  w_grid b_grid grid (c,r+1) in 
    x

(**[mark_territories t] is a matrix representation of the territories with
   "w"/"b" = white/black territory
   "W"/"B" = White/Black stone
   "⋅" = neutral territory *)
let mark_territories t = 
  let w_grid = create_grid t "W" "w" in 
  let b_grid = create_grid t "B" "b" in
  let grid = full_board t in 
  m_t_helper  w_grid b_grid grid (0,0)

let score t =
  let num_prisoners p = 
    let p1_list = p.prisoners in
    float_of_int (List.length p1_list) 
  in  
  let w_grid = create_grid t "W" "w" in 
  let b_grid = create_grid t "B" "b" in 
  let territory_score = score_helper w_grid b_grid 0. 0. (0,0) in 
  let b_score = (fst territory_score) +. num_prisoners t.players.p1 in 
  let w_score = (snd territory_score) +. num_prisoners t.players.p2 
                +. t.config.komi in 
  (b_score, w_score)

let forfeit_message t = 
  if turn t = Black 
  then "Player 1 has forfeit. \nPlayer 2 has won the game!" 
  else "Player 2 has forfeit. \nPlayer 1 has won the game!" 

