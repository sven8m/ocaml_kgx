
type placement = 
	| Point of PointPlacementData.pointPlacementData
	| Grid of GridPlacementData.gridPlacementData
	| Area of AreaPlacementData.areaPlacementData
	| Decorator of DecoratorPlacementData.decoratorPlacementData


let print_placement ff placement = 
	match placement with
	| Point p -> PointPlacementData.print_pointPlacement ff p
	| Grid g -> GridPlacementData.print_gridPlacement ff g
	| Area a -> AreaPlacementData.print_areaPlacement ff a
	| Decorator d -> DecoratorPlacementData.print_decoratorPlacement ff d
(*(*type declaration*)

type placement_type = 
	| Decorator
	| Grid
	| Area

type placement_ref = 
	| Ref | TopLeft | BotRight

type placement_info = {
	p_ref : placement_ref;
	p_point : point;
}

type placement = {
	p_type : placement_type;
	mutable p_info : placement_info list
}


(*declaration *)

let create_placement_info p_ref point = 
	{p_ref = p_ref ; p_point = point}

let create_placement p_type = 
	{p_type = p_type ; p_info = []}

let addPlacementInfo p i =
	p.p_info <- i::p.p_info


(*printing *)


let placement_type_to_string t = 
	match t with
	| Decorator -> "KDecoratorPlacementData"
	| Grid -> "KGridPlacementData"
	| Area -> "KAreaPlacementData"

let print_placement_info ff info =
	let typ = match info.p_ref with
	| Ref -> "referencePoint"
	| TopLeft -> "topLeft"
	| BotRight -> "bottomRight"
	in
	Format.fprintf ff "@[<v 4><%s>@," typ;
	print_point ff info.p_point;
	Format.fprintf ff "@]@,</%s>@," typ

let print_placement ff placement = 
	Format.fprintf ff "@[<v 4><placementData xsi:type=\"krendering:%s\">@," (placement_type_to_string placement.p_type);
	List.iter (print_placement_info ff) placement.p_info;
	Format.fprintf ff "@]@,</placementData>@,"

*)
