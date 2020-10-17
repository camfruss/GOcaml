
(** [score go] is the current score of the game using the territory scoring. The
    first value in the tuple represents the score of the black stones and the 
    second value, the white stones. *)
val score : Game.t -> (float * float)

(** [liberties go pos] is the number of liberties in the group at input [pos]. 
    Requires: a stone is currently placed at [pos].
    *)
val liberties : Game.t -> string -> int

(** [ko go str] is whether an input [str] would violate ko. *)
val ko : Game.t -> string -> bool
