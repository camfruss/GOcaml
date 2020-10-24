open Command
open Game
open OUnit2

(** [cmp_values] is an oUnit Test to determine whether [v1] equals [v2]. *)
let cmp_values name v1 v2 = 
  name >:: (fun _ -> assert_equal v1 v2)

(** [cmp_values_int] is an oUnit Test to determine whether [v1] equals [v2]. *)
let cmp_values_int name v1 v2 =
  name >:: (fun _ -> assert_equal v1 v2 ~printer:string_of_int) 

(** [load_file file] is the Go game representation of the contents in [file]. *)
let load_game file =
  Yojson.Basic.from_file file |> from_json

(* All the game files *)
let standard_19 = load_game "games/standard_19.json"
let game_one = load_game "games/game_one.json"
let corner = load_game "games/corner.json"

let command_tests = [

]

let game_tests = [
  cmp_values "standard board size is 19" 19 (board_size standard_19);
]


let go_tests = [
  (**Liberties tests *)
  (** currently failing tests with stones on the border *)
  cmp_values_int "corner white (0,8) solo, 2 liberties" 
    2 (Go.liberties corner (0,8));
  cmp_values_int "corner white (0,0) solo on corner, 1 liberty"
    1 (Go.liberties corner (0,0));

  cmp_values_int "corner white (8,8), corner string1, 3 liberties"
    3 (Go.liberties corner (8,8));
  cmp_values_int "corner white (7,8), corner string1, 3 liberties" 
    3 (Go.liberties corner (7,8));
  cmp_values_int "corner white (8,7), corner string1, 3 liberties" 
    3 (Go.liberties corner (8,7));

  cmp_values_int "board1 black (3,1) solo, 4 liberties" 
    4 (Go.liberties game_one (3,1));
  cmp_values_int "board1 black (2,3) solo, 3 liberties" 
    3 (Go.liberties game_one (2,3));
  cmp_values_int "board1 white (3,8) solo on edge, 3 liberties" 
    3 (Go.liberties game_one (3,8));


  cmp_values_int "board1 white (1,2) in string" 
    5 (Go.liberties game_one (1,2));
  cmp_values_int "board1 white (2,2) in string, share liberties with (1,2)" 
    5 (Go.liberties game_one (2,2));

  cmp_values_int "board1 white (5,3) in str(5,3)(6,2)(6,3)(6,4), 5 lib" 
    5 (Go.liberties game_one (5,3));
  cmp_values_int "board1 white (6,2) in str(5,3)(6,2)(6,3)(6,4), 5 lib" 
    5 (Go.liberties game_one (6,2));
  cmp_values_int "board1 white (6,3) in str(5,3)(6,2)(6,3)(6,4), 5 lib" 
    5 (Go.liberties game_one (6,3));
  cmp_values_int "board1 white (6,4) in str(5,3)(6,2)(6,3)(6,4), 5 lib" 
    5 (Go.liberties game_one (6,4));




]

let suite =
  "GOcaml Test Suite"  >::: List.flatten [
    game_tests;
    go_tests;
  ]

let _ = run_test_tt_main suite
