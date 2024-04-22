
type vertical = UNDEF | TOP | CENTER | BOTTOM
type horizontal = UNDEF | LEFT | CENTER | RIGHT

class alignment = object 
	val mutable horiz = (UNDEF : horizontal)
	val mutable vert = (UNDEF : vertical)

	method getHorizontalAlignment = horiz
	method getVerticalAlignment = vert

	method setHorizontalAlignment h = horiz <- h
	method setVerticalAlignment v = vert <- v

end

let horizontal_to_string h = match h with
	| UNDEF -> ""
	| LEFT -> "LEFT"
	| CENTER -> "CENTER"
	| RIGHT -> "RIGHT"

let vertical_to_string (v : vertical) = match v with
	| UNDEF -> ""
	| TOP -> "TOP"
	| CENTER -> "CENTER"
	| BOTTOM -> "BOTTOM"

let print_HorizontalAlignment ff a = 
	match a#getHorizontalAlignment with
	| UNDEF -> ()
	| h -> Format.fprintf ff " horizontalAlignment=\"%s\"" (horizontal_to_string h)
	
let print_VerticalAlignment ff a =
	begin match (a#getVerticalAlignment : vertical) with
	| UNDEF -> ()
	| v -> Format.fprintf ff " verticalAlignment=\"%s\"" (vertical_to_string v)
	end
