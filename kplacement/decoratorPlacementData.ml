
class decoratorPlacementData = object
	val mutable absolute = (None : float option)
	val mutable relative = (None : float option)
	val mutable xofs = 0.0
	val mutable yofs = 0.0
	val mutable rot = false
	val mutable width = (None : float option)
	val mutable height = (None : float option)

	method getXOffset = xofs
	method getYOffset = yofs
	method setXOffset x = xofs <- x
	method setYOffset y = yofs <- y

	method isRotateWithLine = rot
	method setRotateWithLine r = rot <- r

	method getWidth = width
	method getHeight = height
	method setWidth w = width <- Some w
	method setHeight h = height <- Some h

	method getAbsolute = absolute
	method getRelative = relative
	method setAbsolute p = 
		absolute <- Some p
	method setRelative p = 
		relative <- Some p
end


let print_decoratorPlacementInfo ff place = 
	Print_utils.print_float_opt "absolute" ff place#getAbsolute;
	Print_utils.print_float_opt "relative" ff place#getRelative;
	Format.fprintf ff " xOffset=\"%f\" yOffset=\"%f\"" place#getXOffset place#getYOffset;
	Print_utils.print_float_opt "width" ff place#getWidth;
	Print_utils.print_float_opt "height" ff place#getHeight;
	if place#isRotateWithLine then
		Format.fprintf ff " rotateWithLine=\"true\""
let print_decoratorPlacement ff place = 
	Format.fprintf ff "@,<placementData xsi:type=\"krendering:KDecoratorPlacementData\"%a/>" print_decoratorPlacementInfo place	
