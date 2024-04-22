class decoratorPlacementData :
	object
		val mutable absolute : float option
		val mutable height : float option
		val mutable relative : float option
		val mutable rot : bool
		val mutable width : float option
		val mutable xofs : float
		val mutable yofs : float
		method getAbsolute : float option
		method getHeight : float option
		method getRelative : float option
		method getWidth : float option
		method getXOffset : float
		method getYOffset : float
		method isRotateWithLine : bool
		method setAbsolute : float -> unit
		method setHeight : float -> unit
		method setRelative : float -> unit
		method setRotateWithLine : bool -> unit
		method setWidth : float -> unit
		method setXOffset : float -> unit
		method setYOffset : float -> unit
	end

val print_decoratorPlacement :
	Format.formatter -> decoratorPlacementData -> unit
(** print decorator placement in kgx format*)