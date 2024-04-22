(*type declarations *)
type color = {
	red : int ; green : int; blue : int}

type coloring = {
	gradient : bool;
	gradient_angle : float;
	color : color;
	target_color : color
}


(*objects creations *)
let create_color red green blue =
	{red ; green ; blue}

let create_coloring ?(grad_angle) ?(target) color =
	match grad_angle , target with
	| None , None ->
		{gradient = false ; gradient_angle = 0.0 ; color = color ; target_color = color}
	| Some g , None ->
		{gradient = true ; gradient_angle = g; color = color ; target_color = color}
	| None , Some t ->
		{gradient = true ; gradient_angle = 0.0 ; color = color ; target_color = t}
	| Some g , Some t ->
		{gradient = true ; gradient_angle = g ; color = color ; target_color = t}


(*printing*)
let print_color ff color t =
	let c = if t then "targetColor" else "color" in
	Format.fprintf ff "@,<%s red=\"%d\" green=\"%d\" blue=\"%d\"/>" c color.red color.green color.blue

