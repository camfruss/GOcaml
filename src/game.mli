
(** TODO *)
type t

(** [from_json json] is the Go game that [json] represents.
    Requires: [json] is a valid json Go representation. *)
val from_json : Yojson.Basic.t -> t

(** [to_json go str] writes the contents of [go] into a valid json.Authors
    Raises: [TODO] if a file already exists. *)
val to_json : t -> string -> unit

(** TODO: require size of board is odd number betwwen 3 and 25 *)
(** TOD *)
val stones : t -> string -> (string * int) list

(** TODO *)
val board_size : t -> int