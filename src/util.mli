
(** [score go] is the current score of the game using the territory scoring. *)
val score : Go.t -> float

(** [liberties go str] is the number of liberties in the group at input [str]. 
    *)
val liberties : Go.t -> string -> int

(** [ko go str] is whether an input [str] would violate ko. *)
val ko : Go.t -> string -> bool
