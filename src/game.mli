
(** [t] is the abstract type of values representing go games. *)
type t

(** [stone] is the type representing the black or white stones *)
type stone = Black | White

(** [InternalError] is raised when the game engine crashes or finds an 
    unexpected and fatal result. *)
exception InternalError

(** [KoException] is raised when the configuration of a Go board is repeated 
    twice in a single game. *)
exception KoException

(** [SelfCaptureException] is raised when a player attempt to place a stone in a
    position that is immediately captured. *)
exception SelfCaptureException

(** [from_json json] is the Go game that [json] represents.
    Requires: [json] is a valid json Go representation. *)
val from_json : Yojson.Basic.t -> t

(** [to_json go str] writes the contents of [go] into a valid json.
    Does not overwrite pre-existing files.. *)
val to_json : t -> string -> unit

(** [in_bounds t (c,r)] is whether the stone at column [c] and row [r] is 
    within the bounds of the board. *)
val in_bounds : t -> (int * int) -> bool

(** [is_empty t pos] is whether there is no stone current at position [pos]
    in the [t]. *)
val is_empty : t -> (int * int) -> bool

(** [score t] is the current score of the game using the territory scoring. The
    first value in the tuple represents the score of the black stones and the 
    second value, the white stones. *)
val score : t -> (float * float)

(** [liberties t pos] is the number of liberties in the group at input [pos]. 
    Requires: a stone is currently placed at [pos]. *)
val liberties : t -> (int * int) -> int

(** [ko t str] is whether an input [str] would violate ko. *)
val ko : t -> (int * int) -> bool

(** [stones t s] is the col and row of every stone on the board of type 
    [stone]. *)
val stones : t -> stone -> (int * int) list

(** [step t move time] is the new game after the current player places a stone 
    on [move] and spends [time] seconds doing so. *)
val step : t -> (int * int) -> int -> t
