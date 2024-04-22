open Point

class areaPlacementData = object
	val mutable topLeft = (None : point option)
	val mutable botRight = (None : point option)

	method setTopLeft p = topLeft <- Some p
	method getTopLeft = topLeft

	method setBotRight p = botRight <- Some p
	method getBotRight =  botRight

end


let print_placement_area ff data = 
	begin match data#getTopLeft with
	| None -> ()
	| Some p ->
		Format.fprintf ff "@,@[<v 4><topLeft>@,%a@]@,</topLeft>" print_point p
	end;
	begin match data#getBotRight with 
	| None -> ()
	| Some p ->
		Format.fprintf ff "@,@[<v 4><bottomRight>@,%a@]@,</bottomRight>" print_point p
	end

let print_areaPlacement ff place =
	Format.fprintf ff "@,@[<v 4><placementData xsi:type=\"krendering:KAreaPlacementData\">";
	print_placement_area ff place;
	Format.fprintf ff "@]@,</placementData>"

