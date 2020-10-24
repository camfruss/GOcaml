open Graphics

(** [interactive] listens for user input. *)
let rec interactive () =
  let event = wait_next_event [Key_pressed] in
  if event.key == 'q' then exit 0
  else interactive ()

(** [setup_background color] changes the background color of the window to 
    [color]. *)
let setup_background color = 
  set_color color;
  fill_rect 0 0 (size_x ()) (size_y ());
  set_color foreground; ()

let main () =
  open_graph " 900x800";
  set_window_title "Go";
  setup_background (rgb 235 195 120);
  interactive ()

let () = main ()
