
(** [in_bounds g c r] is whether the stone at column [c] and row [r] is within 
    the bounds of the board. *)
val in_bounds : Game.t -> (int * int) -> bool

(** [is_empty game pos] is whether there is no stone current at position [pos]
    in the [game]. *)
val is_empty : Game.t -> (int * int) -> bool

(** [score go] is the current score of the game using the territory scoring. The
    first value in the tuple represents the score of the black stones and the 
    second value, the white stones. *)
val score : Game.t -> (float * float)

(** [liberties go pos] is the number of liberties in the group at input [pos]. 
    Requires: a stone is currently placed at [pos]. *)
val liberties : Game.t -> (int * int) -> int

(** [ko go str] is whether an input [str] would violate ko. *)
val ko : Game.t -> (int * int) -> bool
