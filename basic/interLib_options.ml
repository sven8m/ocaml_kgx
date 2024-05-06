let do_rec_viz = ref (-1)
let do_show_link = ref false
let do_show_all = ref false

let set_options rv sl sa = 
	do_rec_viz := rv;
	do_show_link := sl;
	do_show_all := sa
