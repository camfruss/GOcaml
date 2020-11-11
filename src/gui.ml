open Game
open Graphics
open Util

(** [window_size] is the desired nxn dimension of the window which will display 
    the Go board. *)
let window_size = 800

type axis = X | Y

type board_dimensions = {
  mutable board_size : int;
  mutable length : int;
  mutable spacing : int;
}

let b_dims = ref {
    board_size = 0;
    length = 0;
    spacing = 0;
  }

(** [index x s] is the row or column position [x] is closest to. TODO *)
let index x axis = 
  let x' = float_of_int (x - 2 * !b_dims.spacing) in
  let s' = float_of_int !b_dims.spacing in
  let bottom_left = (x' /. s') |> Float.round |> int_of_float in
  match axis with
  | X -> bottom_left
  | Y -> !b_dims.board_size - bottom_left - 1

(** given a row/column, at what coordinate on the screen is this? TODO a*)
let coordinate_of_index idx axis = 
  let loc = if idx < 0 then 0
    else if idx > !b_dims.board_size - 1 then !b_dims.board_size - 1
    else idx 
  in match axis with
  | X -> (loc + 2) * !b_dims.spacing
  | Y -> !b_dims.length - (loc + 2) * !b_dims.spacing

(** [insection x] TODO *)
let intersection x axis = 
  coordinate_of_index (index x axis) axis

(** [draw_stone x y r c] draws a circle at coordiante [(x, y)] with radius [r] 
    and color [c]. *)
let draw_stone x y r c = 
  set_color c;
  fill_circle x y r; ()

(** [setup_background color] changes the background color of the window to 
    [color]. *)
let setup_background color = 
  set_color color;
  fill_rect 0 0 (size_x ()) (size_y ());
  set_color foreground; ()

(** [find_dims bs l] is the value closest to [length] such that shapes can be 
    properly with integer values on a board with [board_size] rows and columns, 
    and the necessary spacing between grid lines for such a drawing to not be 
    malformed. *)
let setup_dims game = 
  let board_size = bounds game in
  let spacing = 
    (float_of_int window_size) /. (3.0 +. float_of_int board_size) 
    |> Float.ceil 
    |> int_of_float in
  let length = spacing * (board_size + 3) in
  b_dims.contents <- {
    board_size = board_size;
    length = length;
    spacing = spacing;
  }; ()

(** [setup_grid board_size s l] is the grid of a board with [board_size] rows 
    and columns, a spacing of [s] between each row and column, and the screen 
    size (i.e. height and width) of [length]. *)
let setup_grid () = 
  let lines = Array.make !b_dims.board_size (0, 0, 0, 0) in
  let s = !b_dims.spacing in
  for i = 0 to Array.length lines - 1 do
    lines.(i) <- ((2 + i) * s, 2 * s, (2 + i) * s, !b_dims.length - 2 * s)
  done;
  let segments = Array.append lines (quartet_swap lines) in 
  draw_segments segments; ()

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
  in label_axis X; label_axis Y; ()

(** [setup_stones game bs s] draws the stones already on the board in [game] on 
    a board with [bs] rows and columns, and spacing [s] between the rows and 
    columns. *)
let setup_stones game = 
  let draw (c,r) color = 
    let x c =  (coordinate_of_index c X) in
    let y r = (coordinate_of_index r Y) in
    draw_stone (x c) (y r) (!b_dims.spacing / 2 - 2) color
  in
  List.iter (fun (c,r) -> draw (c,r) white) (stones game White);
  List.iter (fun (c,r) -> draw (c,r) black) (stones game Black); ()

(** [set_game g] initializes the initial board state given game [g] and returns 
    the calculated spacing between rows and columns. *)
let set_game game = 
  setup_dims game;
  setup_background (rgb 235 195 120);
  setup_grid ();
  setup_axis (); 
  setup_stones game; ()

(** [user_input bs s] listens for user input and updates the game accordingly. 
    [bs] and [s] are needed for properly drawing stones on button presses and 
    represent the number of rows/columns in the board and the spacing between 
    them, respectively. *)
let rec user_input () =
  let event = wait_next_event [Key_pressed; Button_down] in
  match event with
  | {mouse_x; mouse_y; button; keypressed; key} ->
    if keypressed then 
      if event.key == 'q' then exit 0
      else user_input ()
    else if button then
      let actual_x = intersection mouse_x X in
      let actual_y = intersection mouse_y Y in
      let radius = !b_dims.spacing / 2 - 2 in
      draw_stone actual_x actual_y radius white;
      user_input ()

let main () =
  let game = Yojson.Basic.from_file "games/game_one.json" |> from_json in (* TODO: remove *)
  open_graph (Printf.sprintf " %dx%d" window_size window_size);
  set_window_title "Go";
  set_game game;
  user_input ()

let () = main ()
