
(** [t] is the abstract type of values representing an entire Go game. *)
type t

(** [komi] is the compensation awarded to the player who goes second, who is 
    placed at a measurable disadvantage. *)
val komi : float

(** [byoyomi_period] is the length in seconds of each byo-yomi period. *)
val byoyomi_period : int
