open Styles
open Container
open Placement

type container_rendering = {
	mutable container : container;
	mutable data : rendering list
}

and rendering = 
	| ContainerRendering of container_rendering
	| PlacementData of placement
	| ChildArea of rendering list
	| Style of style
	| Action of Actions.action


(*creation*)

let create_container_rendering () = 
	{container = Rect ; data = []}


(*editing*)

let setContainerRendering cr c =
	cr.container <- c

let addContainerData cont d =
	cont.data <- d :: cont.data


(* printing *)

let rec print_container_rendering ff cont = 
	Format.fprintf ff "@[<v 4><children xsi:type=\"krendering:%a>@," print_container cont.container;
	List.iter (print_rendering ff) cont.data;
	Format.fprintf ff "@]@,</children>@,"

and print_child_area ff rl = 
	Format.fprintf ff "@[<v 4><children xsi:type=\"krendering:KChildArea\">@,";
	List.iter (print_rendering ff) rl;
	Format.fprintf ff "@]@,</children>@,"

and print_rendering ff rendering = 
	match rendering with
	| ContainerRendering c ->
		print_container_rendering ff c
	| Style style ->
		print_style ff style
	| PlacementData p ->
		print_placement ff p
	| ChildArea rl ->
		print_child_area ff rl	
	| Action a ->
		Actions.print_action ff a

let print_data ff container data = 
	Format.fprintf ff "@[<v 4><data xsi:type=\"krendering:%a>@," print_container container;
	List.iter (print_rendering ff) data;	
	Format.fprintf ff "@]@,</data>@,"	
