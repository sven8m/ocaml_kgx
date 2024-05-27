(** used to only show the [n] first layers of the visualization *)
let do_rec_viz = ref (-1)

(** if [true] then shows the dependency links *)
let do_show_link = ref false

(** if [true] shows all links *)
let do_show_all = ref false

(** if [true] then parts that can be textual are shown that way *)
let do_textual_opt = ref false

(** [set_options rec_viz show_link show_all textual_opt] *)
let set_options rv sl sa teo = 
	do_rec_viz := rv;
	do_show_link := sl;
	do_show_all := sa;
	do_textual_opt := teo
