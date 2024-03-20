open Styles
open Container
open Placement
open RenderingSig

class data : dataSig = object
	inherit PersistentEntry.propertyHolder
	val mutable contRenderings = []
	val mutable placement = None
	val mutable childArea = None
	val mutable styles = [] 
	val mutable actions = []

	method addContainerRendering cr = 
		contRenderings <- cr :: contRenderings
	method getContainerRenderings = contRenderings

	method getPlacement = placement
	method setPlacement p = placement <- Some p 

	method getChildArea = childArea
	method setChildArea a = childArea <- Some a

	method addStyle a = styles <- a :: styles
	method getStyles = styles

	method addAction a = actions <- a :: actions
	method getActions = actions
end

class containerRendering : containerRenderingSig = object
    inherit data
	val mutable container = Default

	method setContainer c = container <- c
	method getContainer = container
end


(* printing *)

let rec print_container_rendering ff cont = 
	Format.fprintf ff "@[<v 4><children xsi:type=\"krendering:%a>@," print_container cont#getContainer;
	print_data ff (cont :> data);
	print_container_content ff cont#getContainer;
	Format.fprintf ff "@]@,</children>@,"

and print_child_area ff ca = 
	Format.fprintf ff "@[<v 4><children xsi:type=\"krendering:KChildArea\">@,";
	print_data ff ca;
	Format.fprintf ff "@]@,</children>@,"

and print_data ff data =
	
	PersistentEntry.print_properties ff (data :> PersistentEntry.propertyHolder);
	List.iter (print_style ff) data#getStyles;
	begin match data#getPlacement with
	| None -> ()
	| Some p -> print_placement ff p
	end;
	begin match data#getChildArea with
	| None -> ()
	| Some a -> print_child_area ff a
	end;
	List.iter (Actions.print_action ff) data#getActions;
	List.iter (print_container_rendering ff) data#getContainerRenderings 

let print_data_node ff cont = 
	Format.fprintf ff "@[<v 4><data xsi:type=\"krendering:%a>@," print_container cont#getContainer;
	print_data ff (cont:>data);
	print_container_content ff cont#getContainer; 
	Format.fprintf ff "@]@,</data>@,"	
