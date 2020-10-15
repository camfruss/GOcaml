
(** [Deformed] is raised when a player's input is not in the correct format. *)
exception Deformed

(** [Empty] is raised when the players input is empty. *)
exception Empty

(** [KoException] is raised when the configuration of a Go board is repeated 
    twice in a single game. *)
exception KoException

(** [command] is a player's move in a game. They have the option to 
    - Pass
    - Place a stone at a coordinate marked by a character and an integer 
      between 1 and the board size
    - Forfeit
    - Save the current game to the file specified *)
type command = 
  | Pass
  | Play of string * int
  | Forfeit
  | Save of string

(** TODO *)
val parse : string -> command
