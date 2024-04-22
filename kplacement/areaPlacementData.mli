class areaPlacementData :
	object
		val mutable botRight : Point.point option
		val mutable topLeft : Point.point option
		method getBotRight : Point.point option
		method getTopLeft : Point.point option
		method setBotRight : Point.point -> unit
		method setTopLeft : Point.point -> unit
	end

val print_placement_area : Format.formatter -> areaPlacementData -> unit
(** print delimiter of placement in kgx format*)

val print_areaPlacement :
	Format.formatter -> areaPlacementData -> unit
(** print areaPlacement in kgx format*)