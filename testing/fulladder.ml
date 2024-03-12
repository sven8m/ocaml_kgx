open Knode
open Kgraph
open Kport
open Kedge

open Label
open Container
open Rendering
open Styles
open Coloring
open Placement
open Point
let beautiful_node kgraph = 
	let node = new knode kgraph in
	node#setContainer (RoundRect (8.0 , 8.0));
	node#addData (Style (create_style (LineWidth 2.0)));
	let first = create_color 255 255 255 in
	let target = create_color 200 200 250 in
	node#addData (Style (create_style (Background (create_coloring ~grad_angle:90.0 ~target:target first))));
	node#addData (Style (create_style (Shadow (4.0 , 4.0))));
	node


let place () = 
	let p = new AreaPlacementData.areaPlacementData in
	let c1 = create_coord ~pos_val:(Abs 10.0) Left in
	let c2 = create_coord ~pos_val:(Abs 10.0) Top in
	p#setTopLeft (create_point c1 c2);
	(*addPlacementInfo p (create_placement_info TopLeft (create_point c1 c2));
	*)	
	let c3 = create_coord ~pos_val:(Abs 10.0) Right in
	let c4 = create_coord ~pos_val:(Abs 10.0) Bottom in
	(*addPlacementInfo p (create_placement_info BotRight (create_point c3 c4));
	*)
	p#setBotRight (create_point c3 c4);
	Area p

let node_with_text kgraph text = 
	let node = beautiful_node kgraph in
	let d = ContainerRendering {container = Text text ; data = [(Style (create_style Bold)) ; (PlacementData (place ()))]} in
	node#addData d;	
	node

let new_edge kgraph source target = 
	let edge = new kedge kgraph in
	edge#setSource source;
	edge#setTarget target;
	edge#setContainer PolyLine;
	edge#addData (Style (create_style (LineWidth 1.3)));
	edge#addData (Style (create_style ~on_sel:true (LineWidth 2.0)));
	let color = create_color 0 0 255 in
	edge#addData (Style (create_style ~on_sel:true (Foreground (create_coloring color))));
	edge

let centerPlacement x y =
	let p = new PointPlacementData.pointPlacementData in
	let c1 = create_coord ~pos_val:(Rel x) Left in
	let c2 = create_coord ~pos_val:(Rel y) Top in
	p#setRefPoint (create_point c1 c2);
	p#setVerticalAlignment CENTER;
	p#setHorizontalAlignment CENTER;
	Point p


let simpleOpNodeWtT kgraph =
	let node = new knode kgraph in
	node#setContainer Rect;
	node#setWidth 20.0;
	node#setHeight 20.0;
	node#addData (Style (create_style (LineWidth 1.3)));
	node#addData (Style (create_style ~on_sel:true (LineWidth 2.0)));
	let color = create_color 200 200 250 in
	node#addData (Style (create_style (Background (create_coloring color))));
	node#addData (Style (create_style (Shadow (2.0,2.0))));
	node

let simpleOpNode kgraph text relX relY= 
	let node = simpleOpNodeWtT kgraph in
	let d = ContainerRendering {container = Text text ; data = [(Style (create_style Bold) ) ; PlacementData (centerPlacement relX relY)]} in
	node#addData d;
	node


let simpleXorNode kgraph = 
	simpleOpNode kgraph "^" 0.5 0.6

let simpleAndNode kgraph = 
	simpleOpNode kgraph "&amp;" 0.5 0.5

let simpleOrNode kgraph = 
	simpleOpNode kgraph "+" 0.5 0.5

let simpleNandNode kgraph =
	let node = simpleOpNodeWtT kgraph in
	let c1 = create_coord ~pos_val:(Abs 4.0) Left in
	let c2 = create_coord ~pos_val:(Abs 4.0) Top in
	let c3 = create_coord ~pos_val:(Abs 4.0) Right in
	let c4 = create_coord ~pos_val:(Abs 4.0) Bottom in
	let a = new AreaPlacementData.areaPlacementData in
	a#setTopLeft (create_point c1 c2);
	a#setBotRight (create_point c3 c4);
	let d = ContainerRendering {container = Text "nand" ; data = [(Style (create_style Bold)); PlacementData (Area a)]} in
	node#addData d;
	node

let createPort kgraph node text = 
	let port = new kport kgraph in
	port#setNode node;
	let l = new label in
	l#setText text;
	port#addLabel l;
	port

let main _ = 
	let kgraph = new kgraph in

	let main_node = new knode kgraph in
	let d = ChildArea [PlacementData (place ())] in
	let a = Actions.create_actionCollapse () in
	main_node#addData (Action a);
	main_node#addData d;
	main_node#addProperty (PersistentEntry.constraintPortSide ());
	let gray = create_color 224 224 242 in
	main_node#addData (Style (create_style (Background (create_coloring gray))));

	let label1 = new label in
	label1#setText "fulladder";
	label1#setCenter;
	main_node#addLabel label1;

	
	let west = (PersistentEntry.createPortWest ()) in
	let east = (PersistentEntry.createPortEast ()) in

	let port_a = createPort kgraph main_node "a" in
	port_a#addProperty west;
	let port_b = createPort kgraph main_node "b" in
	port_b#addProperty west;
	let port_c = createPort kgraph main_node "c" in
	port_c#addProperty west;
	let port_s = createPort kgraph main_node "s" in
	port_s#addProperty east;
	let port_r = createPort kgraph main_node "r" in
	port_r#addProperty east;

	let xorAB = simpleXorNode kgraph in

	let e1 = new_edge kgraph main_node xorAB in
	e1#setSourcePort port_a; 

	let e2 = new_edge kgraph main_node xorAB in
	e2#setSourcePort port_b; 

	let xorABC = simpleXorNode kgraph in
	ignore (new_edge kgraph xorAB xorABC);

	let e3 = new_edge kgraph main_node xorABC in
	e3#setSourcePort port_c; 

	xorAB#setParent main_node;
	xorABC#setParent main_node;
	
	Format.printf "Wtehwth@.";

	let e4 = new_edge kgraph xorABC main_node in
	e4#setTargetPort port_s;

	ignore (simpleOrNode kgraph);
	ignore (simpleAndNode kgraph);
	ignore (simpleNandNode kgraph);
	
	let andAB = simpleAndNode kgraph in
	andAB#setParent main_node;

	let e = new_edge kgraph main_node andAB in
	e#setSourcePort port_a;

	let e = new_edge kgraph main_node andAB in
	e#setSourcePort port_b;

	let xorAB = simpleXorNode kgraph in
	xorAB#setParent main_node;

	let e1 = new_edge kgraph main_node xorAB in
	e1#setSourcePort port_a; 

	let e2 = new_edge kgraph main_node xorAB in
	e2#setSourcePort port_b; 

	let andABC = simpleAndNode kgraph in
	andABC#setParent main_node;
	
	let e = new_edge kgraph main_node andABC in
	e#setSourcePort port_c;

	ignore (new_edge kgraph xorAB andABC);

	let plus = simpleOrNode kgraph in
	plus#setParent main_node;

	ignore (new_edge kgraph andAB plus);
	ignore (new_edge kgraph andABC plus);

	let e = new_edge kgraph plus main_node in
	e#setTargetPort port_r;

	let oc = open_out "fulladder.kgx" in

	let ff = Format.formatter_of_out_channel oc in
	Kgraph_printer.graph_to_kgx ff kgraph

