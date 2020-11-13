open Game
open Graphics
open Util

(** [window_size] is the desired nxn dimension of the window which will display 
    the Go board. *)
let window_size = 800

(** [axis] represents the columns [X] and rows [Y] of the Go board. This is
    necessary becuase the Graphics module considers (0,0) to be in the bottom 
    left, but our implementation has (0,0) in the top left, meaning only the Y 
    must be inverted. *)
type axis = X | Y

(** [display] represents the type of the contents currently being shown on the 
    window. *)
type display = LoadGame | Handicap | Info | Board | ScoredBoard

type board_dimensions = {
  (** [board_size] is the nxn dimension of the board. *)
  mutable board_size : int;
  (** [length] is the height/width of the window. The window is a square, so we 
      avoid just using height or width to avoid ambiguity. *)
  mutable length : int;
  (** [radius] is the radius of stones placed on the board. *)
  mutable radius : int;
  (** [spacing] is the pixel distance between each row and column. *)
  mutable spacing : int;
}

let b_dims = ref {
    board_size = 0;
    length = 0;
    radius = 0;
    spacing = 0;
  }

(** [setup_background color] changes the background color of the window to 
    [color]. *)
let setup_background color = 
  set_color color;
  fill_rect 0 0 (size_x ()) (size_y ());
  set_color foreground

(** [index p axis] is the line on axis [axis] that position [p] is closest to. 
    On the Go board, lines are indexed top to bottom ([Y] axis), and left to 
    right ([X] axis). *)
let index pos axis = 
  let pos' = float_of_int (pos - 2 * !b_dims.spacing) in
  let spacing = float_of_int !b_dims.spacing in
  let unadjusted_index = (pos' /. spacing) |> Float.round |> int_of_float in
  match axis with
  | X -> unadjusted_index
  | Y -> !b_dims.board_size - unadjusted_index - 1

(** [coordinate_ c r] is window coordinate corresponding to column [c] and row 
    [r] on a Go board. *)
let coordinate c r = 
  let coordinate_of_index idx axis = 
    let loc = 
      if idx < 0 then 0
      else if idx > !b_dims.board_size - 1 then !b_dims.board_size - 1
      else idx 
    in match axis with
    | X -> (loc + 2) * !b_dims.spacing
    | Y -> !b_dims.length - (loc + 2) * !b_dims.spacing
  in (coordinate_of_index c X, coordinate_of_index r Y)

(** [draw_stone x y r c] draws a circle at coordiante [(x, y)], radius [r],
    and color [c]. *)
let draw_stone x y r c = 
  set_color c;
  fill_circle x y r

(** [draw_stone_cr c,r radius color] draws a stone in the [c+1]th column and 
    [r+1]th row with color [color] with radius [radius]. *)
let draw_stone_cr (c,r) radius color = 
  let x, y = coordinate c r in
  draw_stone x y radius color

(** [draw_square_cr] draws a square over the [c+1]th column and [r+1]th row with
    a width and height equal to [length]. *)
let draw_square_cr (c,r) length = 
  set_color black;
  let x, y = coordinate c r in
  draw_rect (x - length / 2) (y - length / 2) length length 

(** [draw_ring x y c1 c2] draws a ring at coordiante [(x, y)] with inner color 
    [c1] and outer color [c2]. *)
let draw_ring x y c1 c2 = 
  let r1 = !b_dims.radius / 2 in
  let r2 = r1 + 3 in
  draw_stone x y r2 c2;
  draw_stone x y r1 c1

(** [prev_ring game] draws or removes a ring on the last placed stone. *)
let prev_ring game style =
  let c, r = last_stone game in
  let x, y = coordinate c r in
  match style with
  | `Remove -> begin
      if c >= 0 && r >= 0 then 
        match turn game with
        | Black -> draw_stone x y !b_dims.radius white
        | White -> draw_stone x y !b_dims.radius black
      else ()
    end
  | `Draw -> begin 
      match turn game with
      | Black -> draw_ring x y white black
      | White -> draw_ring x y black white
    end

(** [setup_dims game] determines the optimal window dimensions and line spacing 
    for the graphical user interface given game [game]. 
    Requires: this function must be called before all other game or window 
      specific functions. *)
let setup_dims game = 
  let board_size = bounds game in
  let spacing = 
    (float_of_int window_size) /. (3.0 +. float_of_int board_size) 
    |> Float.ceil 
    |> int_of_float in
  let length = spacing * (board_size + 3) in
  let radius = spacing / 2 - 2 in
  b_dims.contents <- {
    board_size = board_size;
    length = length;
    radius = radius;
    spacing = spacing;
  }

(** [setup_grid] is the basic grid layout with dimensions determined previously 
    in [setup_dims]. *)
let setup_grid () = 
  let lines = Array.make !b_dims.board_size (0, 0, 0, 0) in
  let s = !b_dims.spacing in
  for i = 0 to Array.length lines - 1 do
    lines.(i) <- 
      ((2 + i) * s, 2 * s, 
       (2 + i) * s, !b_dims.length - 2 * s)
  done;
  let segments = Array.append lines (quartet_swap lines) in 
  draw_segments segments

(** [setup_axis] is the axis labels for the game as specified in 
    [setup_dims]. *)
let setup_axis () = 
  set_font "-*-Helvetica-medium-r-normal--18-*-*-*-*-*-iso8859-1";
  let label_axis axis = 
    for j = 0 to 1 do
      let offset = if j = 0 then 0 else !b_dims.length in
      for i = 0 to !b_dims.board_size - 1 do
        let loc = ((2 + i) * !b_dims.spacing), (offset - !b_dims.spacing |> Int.abs) in
        let s = match axis, loc with
          | X, (x, y) ->
            let y_offset = if j = 1 then 16 else 5 in
            moveto (x - 5) (y - y_offset); 
            Char.escaped (Char.chr (65 + i))
          | Y, (x, y) -> 
            let x_offset = if j = 1 then 10 else 5 in
            moveto (y - x_offset) (x - 9); 
            string_of_int (!b_dims.board_size - i)
        in draw_string s;
      done
    done;
  in label_axis X; label_axis Y

let star_locations () = 
  let n1 = 
    Float.floor ((float_of_int !b_dims.board_size -. 1.0) /. 12.0) +. 2.0 
    |> int_of_float in
  let n2 = !b_dims.board_size - (n1 + 1) in
  let n3 = 
    float_of_int !b_dims.board_size /. 2.0 
    |> Float.floor 
    |> int_of_float 
  in (n1, n2, n3)

(** [setup_stars] places the reference points, sometimes called stars, on the 
    Go board as specified in [setup_dims]. *)
let setup_stars () = 
  let n1, n2, n3 = star_locations () in
  let stars = 
    if !b_dims.board_size <= 7 then cartesian [n1; n2] [n1; n2]
    else if !b_dims.board_size <= 13 then 
      (n3, n3) :: cartesian [n1; n2] [n1; n2]
    else cartesian [n1; n2; n3] [n1; n2; n3]
  in
  let coordinates = List.map (fun (c, r) -> coordinate c r) stars 
  in List.iter (fun (x, y) -> draw_stone x y 3 black) coordinates

(** [setup_handicap g h] places [h] stones on a Go board at the beginning of a 
    Go game. 
    Requires: 2 <= [h] <= 9. *)
let setup_handicap game h =
  let h_positions = ref [] in
  let add lst = 
    h_positions := !h_positions @ lst 
  in
  let n1, n2, n3 = star_locations () in
  if h >= 8 then add [(n3, n1); (n3, n2)];
  if h >= 6 then add [(n1, n3); (n2, n3)];
  if h >= 5 && h mod 2 = 1 then add [(n3, n3)];
  if h >= 4 then add [(n1, n1); (n2, n2)];
  if h = 3 then add [(n2, n2)];
  if h >= 2 then add [(n1, n2); (n2, n1)];
  List.iter 
    (fun (c,r) -> 
       draw_stone_cr (c,r) (!b_dims.spacing / 2 - 2) black
    ) !h_positions;
  handicap game !h_positions

(** [setup_stones game] draws the stones already on the board in [game]. *)
let setup_stones game = 
  let rad = (!b_dims.spacing / 2 - 2) in
  if stones game Black = [] then setup_handicap game 5 else (** TODO: put actual number in *)
    (List.iter (fun (c,r) -> draw_stone_cr (c,r) rad white) (stones game White);
     List.iter (fun (c,r) -> draw_stone_cr (c,r) rad black) (stones game Black);
     prev_ring game `Draw; game)

let set_score game = 
  let grid = fill_grid game in
  for r = 0 to Array.length grid - 1 do
    for c = 0 to Array.length grid.(r) - 1 do 
      match grid.(r).(c) with
      | BlackT -> draw_stone_cr (c, r) 4 black
      | WhiteT -> draw_stone_cr (c, r) 4 white
      | Neutral -> draw_square_cr (c, r) 16 
      | Empty | BlackS | WhiteS -> ()
    done
  done

(** [set_game g] initializes the initial board state given game [g]. *)
let set_game game = 
  setup_dims game;
  clear_graph ();
  setup_background (rgb 235 195 120);
  setup_grid ();
  setup_axis (); 
  setup_stars ();
  setup_stones game

(** [set_info game] displays all the revelent information for game [game]. *)
let set_info game = 
  clear_graph ();
  setup_background (rgb 235 195 120)

let set_config game = 
  setup_background (rgb 235 195 120); ()
(* Field to load file *)
(* field to set handicap, default is 1 *)

(** [user_input game t] listens for user input and updates the game accordingly. 
*)
let rec user_input game display t0 =
  let event = wait_next_event [Key_pressed; Button_down] in
  match event with
  | {mouse_x; mouse_y; button; keypressed; key} ->
    if keypressed then 
      match event.key, display with 
      | ('i', Info) -> user_input (set_game game) Board t0
      | ('i', Board) -> set_info game; user_input game Info t0
      | ('q', _) -> exit 0
      | ('s', Info) -> to_json game "./games/hard_score.json"; exit 0 (* TODO: give an actual name *)
      | ('s', Board) -> set_score game; user_input game ScoredBoard t0
      | ('s', ScoredBoard) -> ignore (set_game game); user_input game Board t0
      | _ -> user_input game Board t0
    else if button  && display != Info && display != ScoredBoard then
      let column = index mouse_x X in
      let row = index mouse_y Y in
      let actual_x, actual_y = coordinate column row in
      let t1 = time () in
      try
        let game' = step game (column, row) (t1 - t0) in
        let (c1,c2) = match turn game with
          | White -> white, black
          | Black -> black, white
        in
        draw_stone actual_x actual_y !b_dims.radius c1;
        draw_ring actual_x actual_y c1 c2;
        prev_ring game `Remove;
        ignore (set_game game');
        user_input game' Board t1
      with 
      | KoException -> user_input game Board t0
      | SelfCaptureException -> user_input game Board t0
      | StoneAlreadyExistsException -> user_input game Board t0
      | TimeExpiredException -> () (* TODO: implement *)
    else match display with 
      | Info -> user_input game Info t0
      | ScoredBoard -> user_input game ScoredBoard t0
      | d -> user_input game d t0 

let main () =
  let game = Yojson.Basic.from_file "games/easy_score.json" |> from_json in (* TODO: load user-defined game *)
  open_graph (Printf.sprintf " %dx%d" window_size window_size);
  set_window_title "GOcaml";
  user_input (set_game game) Board (time ())

let () = main ()
