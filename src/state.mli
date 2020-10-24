
(** The abstract type of values representing the dynamic components of a game 
    state. *)
type t 

(** [init_state a] is the initial state of the game when playing adventure [a]. 
    In that state the adventurer is currently located in the starting room,
    and they have visited only that room. *)
val init_state : Game.t -> t

(** [stones t s] is the col and row of every stone on the board of type 
    [stone]. *)
val stones : t -> Game.stone -> (int * int) list

(** [is_empty state pos] is whether there is no stone current at position [pos]
    on the board in [state]. *)
val is_empty : t -> (int * int) -> bool


(** The type representing the result of an attempted Go board update. *)
type result = Legal of t | Illegal

(** [play move game st] is [r] if attempting the to place a stone given by 
    [move] in game [game] results in [r]. If there is no stone in that position,
    then [r] is [Legal st'], where [st'] is the new board state. Otherwise the
    result is [Illegal].

    Effects: none.  [play] is not permitted to do any printing. *)
val play : (int * int) -> Game.t -> t -> result

(** [turn_start_text] returns the string of tezt to be displayed at the start
    of a turn. *)
val turn_start_text : Game.t -> t -> string

