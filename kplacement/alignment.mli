type vertical = UNDEF | TOP | CENTER | BOTTOM
(** vertical alignment type*)

type horizontal = UNDEF | LEFT | CENTER | RIGHT
(** horizontal alignment type*)

class alignment :
	object
		val mutable horiz : horizontal
		val mutable vert : vertical
		method getHorizontalAlignment : horizontal
		method getVerticalAlignment : vertical
		method setHorizontalAlignment : horizontal -> unit
		method setVerticalAlignment : vertical -> unit
	end

val print_HorizontalAlignment :
	Format.formatter -> alignment -> unit
(**print horizontal alignment in kgx format *)

val print_VerticalAlignment :
	Format.formatter -> alignment -> unit
(** print vertical alignment in kgx format *)

