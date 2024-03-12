open Point
open Alignment
open Print_utils

class pointPlacementData = object 
	inherit alignment
	val mutable refPoint = (None : point option)
	val mutable minHeight = (None : float option)
	val mutable minWidth = (None : float option)

	method getRefPoint = refPoint
	method setRefPoint p = refPoint <- Some p

	method getMinHeight = minHeight
	method setMinHeight h = minHeight <- Some h

	method getMinWidth = minWidth
	method setMinWith w = minWidth <- Some w
end


let print_PointPlacementInformations ff place = 
	Format.fprintf ff "%a%a%a%a" 
		(print_float_opt "minHeight") place#getMinHeight
		(print_float_opt "minWidth") place#getMinWidth
		print_VerticalAlignment place
		print_HorizontalAlignment place

let print_pointPlacement ff place = 
	Format.fprintf ff "@[<v 4><placementData xsi:type=\"krendering:KPointPlacementData\" %a>@," print_PointPlacementInformations place;
	begin match place#getRefPoint with
	| None -> ()
	| Some p ->
		Format.fprintf ff "@[<v 4><referencePoint>@,%a@]@,</referencePoint>" print_point p
	end;
	Format.fprintf ff "@]@,</placementData>@,"
