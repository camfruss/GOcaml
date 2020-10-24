open Command
open Game

let welcome_message = 
  {|
  ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■
  ■           WELCOME TO GOCAML           ■
  ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■
  
  Supported Moves
  - play <position>
    make sure the position is a single character followed by a number between 1
    and the board size specified in the game file :)
  - quit
    for when you are done playing :(

  If you ever want to quit the terminal, press ^D (control D).
  |}

let exit_message = "We hope you enjoyed playing GOCaml and come back soon!"

(** [play game] game while command isn't to quit. If command is play, update state. *)
let rec play game = 
  print_string "> ";
  let user_input = read_line () in 
  try 
    match parse game user_input with
    | Quit -> print_endline exit_message; exit 0
    | Pass -> failwith "unimplemented"
    | Forfeit -> failwith "unimplimented"
    | Play pos -> play (step game (istone_pos pos) 0)
    | Save s -> begin
      let exists = Sys.file_exists s in 
      if not exists then to_json game s; 
      print_endline "A file with this name already exists."; 
      print_endline "Please choose a different name."; play game
    end
  with 
    | Empty -> 
      print_endline "You didn't type anything! Try again!"; play game
    | Deformed -> 
      print_endline "That's not a valid command!"; play game
    | GoOutOfBounds ->
      print_endline "The position is out of the game bounds"; play game
    | StoneAlreadyExists ->
      print_endline "A stone already exists in that location."; play game

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  print_endline welcome_message;
  print_string "Please enter the name of the game file you wish to load.\n> ";
  let rec init () = 
    match read_line () with
    | exception End_of_file -> ()
    | f ->
      try play (Yojson.Basic.from_file f |> from_json) with 
      | Sys_error _ -> 
        print_string "Please enter a valid GOCaml file.\n> "; init ()
  in init ()

(* Execute the game engine. *)
let () = main ()
