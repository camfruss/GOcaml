
(** [Deformed] is raised when a player's input is not in the correct format. *)
exception Deformed

(** [Empty] is raised when the players input is empty. *)
exception Empty

(** [KoException] is raised when the configuration of a Go board is repeated 
    twice in a single game. *)
exception KoException

(** [command] is a player's move in a game. They have the option to 
    - Pass
    - Play a stone at a coordinate marked by a character and an integer 
      between 1 and the board size.
        Example: "A1" places a stone in the top left corner. 
    - Forfeit
    - Save the current game to the file specified 
    - Quit will quit the game application *)
type command = 
  | Pass
  | Play of string
  | Forfeit
  | Save of string
  | Quit

(** [parse game str] parses a player's input into a [command]. The verb must be
    all lowercase and the string following the verb is must adhere to the above 
    specifications for [command].
    
    Raises: [Empty] if [str] does not contain alphanumeric characters. 
    Raises: [Malformed] if the command is malformed. 
    
    Requires: [str] contains alphanumerics and spaces. *)
val parse : Game.t -> string -> command

(** [istone_pos pos] is the integer position of [pos].
    Requires: [pos] is a single capitalized alphabetic character followed by 
    integer in the interval [1, board_size]. *)
val istone_pos : string -> (int * int)
