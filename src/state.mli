(** 
   Representation of dynamic Go board state.

   This module represents the state of an Go board as it is being played,
   including the adventurer's current room, the rooms that have been visited,
   and functions that cause the state to change.
*)

(** The abstract type of values representing the game state. *)
type t 

(** [init_state a] is the initial state of the game when playing adventure [a]. 
    In that state the adventurer is currently located in the starting room,
    and they have visited only that room. *)
val init_state : Game.t -> t


(**
   board.white (move array)
   board.black (move array)
   -move array

   players.p#.prisoners (int array)
*)



(** The type representing the result of an attempted Go board update. *)
type result = Legal of t | Illegal

(** [play move game st] is [r] if attempting the to place a stone given by 
    [move] in game [game] results in [r]. If there is no stone in that position,
    then [r] is [Legal st'], where [st'] is the new board state. Otherwise the
    result is [Illegal].

    Effects: none.  [play] is not permitted to do any printing. *)
val play : (int * int) -> Game.t -> t -> result



(* go northeast

   go exit_name

   exitname -> result

   play A1 *)