open Command
open Game
open OUnit2

(** [cmp_values] is an oUnit Test to determine whether [v1] equals [v2]. *)
let cmp_values name v1 v2 = 
  name >:: (fun _ -> assert_equal v1 v2)

(** [test_raises2 n f i1 i2 e] is on OUnit Test to determine whether 
    [func i1 i2] raises [error].  *)
let test_raises2 name func input1 input2 error = 
  name >:: (fun _ -> assert_raises error (fun () -> func input1 input2))

let error_free name expr = 
  name >:: (fun _ -> expr)

(** [load_file file] is the Go game representation of the contents in [file]. *)
let load_game file =
  Yojson.Basic.from_file file |> from_json

(* All the game files *)
let standard_19 = load_game "games/standard_19.json"
let game_one = load_game "games/game_one.json"
let corner = load_game "games/corner.json"

let command_tests = [
  (* Converting string location to integer tuple *)
  cmp_values "A1 is (0, 0)" (0, 0) (istone_pos "A1");
  cmp_values "C3 is (2, 2)" (2, 2) (istone_pos "C3");
  cmp_values "A13 is (0, 12)" (0, 12) (istone_pos "A13");

  (* Normal Parse Tests *)
  cmp_values "play A1 is Play A1" (parse standard_19 "play A1") (Play "A1");
  cmp_values "' quit ' is Quit" (parse standard_19 " quit ") Quit;
  cmp_values "forfeit is Forfeit" (parse standard_19 "forfeit") Forfeit;
  cmp_values "pass is Pass" (parse standard_19 "pass") Pass;
  cmp_values "save file.json is Save 'file.json'" 
    (parse standard_19 "save file.json") (Save "file.json");

  (* Parse Exception Tests *)
  test_raises2 "Deformed Exception" parse standard_19 "pLaY A2" Deformed;
  test_raises2 "Empty Exception" parse standard_19 " " Empty;
  test_raises2 "StoneAlreadyExists Exception" parse corner "play A1" 
    StoneAlreadyExists;
  test_raises2 "GoOutOfBounds Exception" parse standard_19 "play Z1" 
    GoOutOfBounds;
  test_raises2 "GoOutOfBounds Exception" parse standard_19 "play A20" 
    GoOutOfBounds;
]

let liberty_tests = [
  cmp_values "corner white (0,8) solo, 2 liberties" 
    2 (liberties corner (0,8));
  cmp_values "corner white (0,0) solo on corner, 1 liberty"
    1 (liberties corner (0,0));

  cmp_values "corner white (8,8), corner string1, 3 liberties"
    3 (liberties corner (8,8));
  cmp_values "corner white (7,8), corner string1, 3 liberties" 
    3 (liberties corner (7,8));
  cmp_values "corner white (8,7), corner string1, 3 liberties" 
    3 (liberties corner (8,7));

  cmp_values "board1 black (3,1) solo, 4 liberties" 
    4 (liberties game_one (3,1));
  cmp_values "board1 black (2,3) solo, 3 liberties" 
    3 (liberties game_one (2,3));
  cmp_values "board1 white (3,8) solo on edge, 3 liberties" 
    3 (liberties game_one (3,8));

  cmp_values "board1 white (1,2) in string" 
    5 (liberties game_one (1,2));
  cmp_values "board1 white (2,2) in string, share liberties with (1,2)" 
    5 (liberties game_one (2,2));

  cmp_values "board1 white (5,3) in str(5,3)(6,2)(6,3)(6,4), 5 lib" 
    5 (liberties game_one (5,3));
  cmp_values "board1 white (6,2) in str(5,3)(6,2)(6,3)(6,4), 5 lib" 
    5 (liberties game_one (6,2));
  cmp_values "board1 white (6,3) in str(5,3)(6,2)(6,3)(6,4), 5 lib" 
    5 (liberties game_one (6,3));
  cmp_values "board1 white (6,4) in str(5,3)(6,2)(6,3)(6,4), 5 lib" 
    5 (liberties game_one (6,4));
]

let file_tests = [
  error_free "corner -> corner.json without errors" 
    (to_json corner "./tests/supporting/corner.json");
  cmp_values "./tests/supporting/corner.json and original corner are equal"
    (load_game "./tests/supporting/corner.json") corner
]

(* GAME STATE WAS TESTED MANUALLY *)

let game_tests = [
  (* In Bounds Tests *)
  cmp_values "standard_19 0,0 in bounds" (in_bounds standard_19 (0,0)) true;
  cmp_values "standard_19 18,18 in bounds" (in_bounds standard_19 (18,18)) true;
  cmp_values "standard_19 -1,-1 not in bounds" (in_bounds standard_19 (-1,-1)) 
    false;
  cmp_values "standard_19 19,19 not in bounds" (in_bounds standard_19 (19,19)) 
    false;

  (* Is Empty Tests *)
  cmp_values "standard_19 F8 is empty" (is_empty standard_19 (5,7)) true;
  cmp_values "corner A1 is not empty" (is_empty corner (0,0)) false;

  (* Scoring Tests *)
  cmp_values "corner score" (score corner) (0.0,5.5);
  cmp_values "standard_19 score" (score standard_19) (0.0, 6.5);


]

let suite =
  "GOcaml Test Suite"  >::: List.flatten [
    command_tests;
    liberty_tests;
    file_tests;
    game_tests;
  ]

let _ = run_test_tt_main suite
