

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
