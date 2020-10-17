
(** TODO: require size of board is odd number betwwen 3 and 25 *)

(** [t] is the abstract type of values representing go games. *)
type t

(** [stone] is the type representing the black or white stones *)
type stone = Black | White

(** [from_json json] is the Go game that [json] represents.
    Requires: [json] is a valid json Go representation. *)
val from_json : Yojson.Basic.t -> t

(** [to_json go str] writes the contents of [go] into a valid json.Authors
    Raises: [TODO] if a file already exists. *)
val to_json : t -> string -> unit

(** [stones t s] is the stones on the board of type [stone]. *)
val stones : t -> stone -> (int * int) list

(** [board_size t] is the nxn dimension of the board in game [t]. *)
val board_size : t -> int
