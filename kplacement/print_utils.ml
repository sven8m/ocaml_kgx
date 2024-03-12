
let print_float_opt key ff value = 
	match value with
	| None -> ()
	| Some value -> Format.fprintf ff "%s=\"%f\" " key value

let print_string_opt key ff value = 
	match value with
	| None -> ()
	| Some value -> Format.fprintf ff "%s=\"%s\" " key value
