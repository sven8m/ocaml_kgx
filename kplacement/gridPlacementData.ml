open AreaPlacementData

class gridPlacementData = object
	inherit areaPlacementData
	val mutable minWidth = (None : float option)
	val mutable minHeight = (None : float option)
	
	method getMinWidth = minWidth
	method setMinWidth w = minWidth <- w

	method getMinHeight = minHeight
	method setMinHeight h = minHeight <- h

end


let print_gridPlacementInfo ff data = 
	begin match data#getMinWidth with
	| None -> ()
	| Some f -> Format.fprintf ff " minCellWidth=\"%f\"" f
	end;
	begin match data#getMinHeight with
	| None -> ()
	| Some f -> Format.fprintf ff " minCellHeight=\"%f\"" f
	end

let print_gridPlacement ff place = 
	Format.fprintf ff "@,@[<v 4><placementData xsi:type=\"krendering:KGridPlacementData\"%a>" print_gridPlacementInfo place;
	print_placement_area ff (place :> areaPlacementData);
	Format.fprintf ff "@]@,</placementData>"

