


class obj_pos = object
	val mutable xpos = (None : float option)
	val mutable ypos = (None : float option)
	val mutable width = (None : float option)
	val mutable height = (None : float option)
	
	method setXpos x = xpos <- Some x
	method setYpos y = ypos <- Some y
	method setWidth w = width <- Some w
	method setHeight h = height <- Some h

	method getXpos = xpos
	method getYpos = ypos
	method getWidth = width
	method getHeight = height

end

let print_float_opt message ff value = 
	match value with
	| None -> ()
	| Some f -> Format.fprintf ff " %s=\"%f\"" message f

let print_obj_pos ff obj =
	Format.fprintf ff "%a%a%a%a" 
		(print_float_opt "xpos") obj#getXpos 
		(print_float_opt "ypos") obj#getYpos
		(print_float_opt "width") obj#getWidth
		(print_float_opt "height") obj#getHeight

