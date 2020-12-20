open Command
open Game
open Util


let welcome_message = 
  {|
  ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■
  ■           WELCOME TO GOCAML           ■
  ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■

  Supported Moves
  - play <position>
    make sure the position is a single uppercase letter followed by a number 
    between 1 and the board size specified in the game file :)
  - quit
    for when you are done playing :(
  - save <filename>
    for when you are done playing but want to save your current progress
  - print
    to print the stones currently on the board
  - score
    to print the current score 
  - undo
    to undo the previous player's move

  If you ever want to quit the terminal, press ^D (control D).
  |}

let exit_message = "We hope you enjoyed playing GOCaml and come back soon!"

let forfeit_message game = 
  match turn game with
  | Black -> "Player 1 has forfeit. \nPlayer 2 has won the game!" 
  | White -> "Player 2 has forfeit. \nPlayer 1 has won the game!" 

let score_str (a,b) = 
  "player 1: " ^ string_of_float(a) ^ "\nplayer 2: " 
  ^ string_of_float(b)

(** [play game t] manages each turn, parsing input, and displaying helpful 
    information to the terminal. [t] is the UNIX time this move started. *)
let rec play game t0 = 
  let name = if turn game = White then "W" else "B" in
  print_string (name ^ " > ");
  let user_input = read_line () in 
  try 
    let t1 = time () in
    match parse game user_input with
    | Quit -> print_endline exit_message; exit 0
    | Pass -> play (step game None 0) t1
    | Print -> print_endline (string_of_board game); play game t0
    | Score -> print_endline (score_str (score game)); play game t0
    | Forfeit -> print_endline (forfeit_message game); exit 0
    | Play pos -> play (step game (Some (istone_pos pos)) (t1 - t0)) t1
    | Undo -> play (undo game) t0
    | Save s -> begin
        let exists = Sys.file_exists s in 
        if not exists then to_json game s 
        else (print_endline "A file with this name already exists."; 
              print_endline "Please choose a different name."; play game t0)
      end
  with 
  | Empty -> 
    ANSITerminal.(print_string [red] "You didn't type anything! Try again! \n"); 
    play game t0
  | Deformed -> 
    ANSITerminal.(print_string [red] "That's not a valid command! \n");
    play game t0
  | GoOutOfBounds ->
    ANSITerminal.(
      print_string [red] "The position is out of the game bounds \n"); 
    play game t0
  | StoneAlreadyExists ->
    ANSITerminal.(
      print_string [red]"A stone already exists in that location. \n");
    play game t0

(** [main] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.(
    print_string [green] welcome_message;
    print_string [green]
      "Please enter the name of the game file you wish to load.";
    print_string [default] "\n>");
  let rec init () = 
    match read_line () with
    | exception End_of_file -> ()
    | f ->
      try play (Yojson.Basic.from_file f |> from_json) (time ()) with 
      | Sys_error _ -> 
        ANSITerminal.
          (print_string [red] "Please enter a valid GOCaml file." ;
           print_string [default] "\n> "); init ()
  in init ()

let () = main ()
