



type pos_type =
	| Left | Right | Top | Bottom

type pos_val = 
	| Undef | Abs of float | Rel of float


type coord = {
	c_type : pos_type;
	c_val : pos_val;
}

type point = {
	x : coord;
	y : coord
}

let create_coord ?(pos_val = Undef) pos_type = 
	{c_type = pos_type ; c_val = pos_val}

let create_point x y = 
	{x = x ; y = y}


let coord_type_to_string c =
	match c with
	| Left -> "KLeftPosition"
	| Right -> "KRightPosition"
	| Top -> "KTopPosition"
	| Bottom -> "KBottomPosition"

let print_pos_val ff pos = 
	match pos with
	| Undef -> ()
	| Abs f ->
		Format.fprintf ff "absolute=\"%f\"" f
	| Rel f ->
		Format.fprintf ff "relative=\"%f\"" f

let print_coord ff coord v =
	Format.fprintf ff "<%s xsi:type=\"krendering:%s\" %a/>@," v (coord_type_to_string coord.c_type) print_pos_val coord.c_val

let print_point ff point = 
	print_coord ff point.x "x";
	print_coord ff point.y "y";
