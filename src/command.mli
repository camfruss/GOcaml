
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
        Example: 
    - Forfeit
    - Save the current game to the file specified 
    - Quit will quit the game application *)

type command = 
  | Pass
  | Play of string
  | Forfeit
  | Save of string
  | Quit

(** [parse gamme str] parses a player's input into a [command]. The first
    word (i.e., consecutive sequence of non-space characters) of [str] becomes 
    the verb. The rest of the words, if any, become a string.
    Examples: 
    - [parse "    Play  A1   "] is [Play "A1"]
    - [parse "quit"] is [Quit]. 

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space 
    characters (only ASCII character code 32; not tabs or newlines, etc.).

    Raises: [Empty] if [str] is the empty string or contains only spaces. 

    Raises: [Malformed] if the command is malformed. A command
    is {i malformed} if the verb is neither "quit", "play", "pass", "save", or
        "forfeit",
    or if the verb is "quit", "pass", or "forfeit" and there is a non-empty str
    or if the verb is "play" or "save" and there is an empty string after.*)
val parse : Game.t -> string -> command

(** [istone_pos pos] is the integer position of [pos] TODO
    Requires: [pos] is a single capitalized alphabetic character followed by 
    integer in the interval [1,board_size]. *)
val istone_pos : string -> (int * int)
