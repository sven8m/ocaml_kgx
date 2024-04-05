open Knode
open Kgraph
open Container
open Rendering
open Styles
open Coloring
open Placement
open Point
open Kport
open Label
open Kedge

type op_node = {
	node : knode;
	inputs : kport list;
	outputs : kport list;
	control : kport list
}


type endPoint = {
	node : knode;
	port : kport;
	mutable labels : string list;
}
(*
type utils = {	
	function_node : knode;
	layer : int;
	count_env : int Ast.IdentEnv.t;
	node_outputs : Ast.IdentSet.t;
	node_inputs : Ast.IdentSet.t;
	in_reg : bool;
	in_match_eq : bool;
	var_decs : Ast.ty Ast.IdentEnv.t;
}

type env = {
	input_env : endPoint Ast.IdentEnv.t;
	output_env : endPoint Ast.IdentEnv.t;
}
*)

let place ?(top=15.0) ?(left=10.0) ?(right=10.0) ?(bottom=10.0) () =
	let p = new AreaPlacementData.areaPlacementData in
	let c1 = create_coord ~pos_val:(Abs top) Left in
	let c2 = create_coord ~pos_val:(Abs left) Top in
	p#setTopLeft (create_point c1 c2);
	let c3 = create_coord ~pos_val:(Abs right) Right in
	let c4 = create_coord ~pos_val:(Abs bottom) Bottom in
	p#setBotRight (create_point c3 c4);
	Area p

let absolutePointPlacement ?(minW) ?(minH) x y = 
	let p = new PointPlacementData.pointPlacementData in
	let c1 = create_coord ~pos_val:(Abs x) Left in
	let c2 = create_coord ~pos_val:(Abs y) Top in
	p#setRefPoint (create_point c1 c2);
	p#setVerticalAlignment CENTER;
	p#setHorizontalAlignment CENTER;
	begin match minW with
	| None -> ()
	| Some w -> p#setMinWidth w
	end;
	begin match minH with
	| None -> ()
	| Some h -> p#setMinHeight h
	end;
	Point p

let centerPlacement ?(ho_mar=None) ?(ver_mar=None) x y =
	let p = new PointPlacementData.pointPlacementData in
	let c1 = create_coord ~pos_val:(Rel x) Left in
	let c2 = create_coord ~pos_val:(Rel y) Top in
	p#setRefPoint (create_point c1 c2);
	p#setVerticalAlignment CENTER;
	p#setHorizontalAlignment CENTER;
	begin match ho_mar with
	| None -> ()
	| Some h -> p#setHorizontalMargin h
	end;
	begin match ver_mar with
	| None -> ()
	| Some v -> p#setVerticalMargin v
	end;
	Point p

let defaultNode ?(part="1") ?(order=false) kgraph = 
	let node = new knode kgraph in
	node#setWidth 20.0;
	node#setHeight 20.0;	
	node#addProperty (PersistentEntry.portAlignmentNorth "CENTER");
	node#addProperty (PersistentEntry.portAlignmentSouth "CENTER");
	node#addProperty (PersistentEntry.partition part);
	if order then
		node#addProperty (PersistentEntry.constraintPortOrder ())
	else
		node#addProperty (PersistentEntry.constraintPortSide ());
	node#addProperty (PersistentEntry.allowSwitch ()); 
	node

let defaultStateNode ?(part="1") kgraph = 
	let node = new knode kgraph in
	node#setWidth 20.0;
	node#setHeight 20.0;
	node#addProperty (PersistentEntry.partition part);
	node


let simpleOpContWtT () =
	let cont = new containerRendering in
	cont#setContainer Rect;
	cont#addStyle (create_style (LineWidth 1.3));
	cont#addStyle (create_style ~on_sel:true (LineWidth 2.0));
	let color = create_color 200 200 250 in
	cont#addStyle (create_style (Background (create_coloring color)));
	cont#addStyle (create_style (Shadow (2.0,2.0)));
	cont

let simpleOpNodeWtT kgraph =
	let node = defaultNode kgraph in
	node#addData (simpleOpContWtT ());
	node


let simpleText ?(s=11) ?(ho_mar=0.0) ?(ver_mar) text relX relY =
	let c = new containerRendering in
	c#setContainer (Text text);
	c#addStyle (create_style ~on_sel:true Bold);
	c#addStyle (create_style (FontSize s));
	c#setPlacement (centerPlacement ~ho_mar:(Some ho_mar) ~ver_mar:ver_mar relX relY);
	c

let invisiblePort kgraph knode = 
	let port = new kport kgraph in
	port#setNode knode;
	port#setHeight (-0.1);
	port#setWidth (-0.1);
	let cont = new containerRendering in
	cont#addStyle (create_style Invisibility);
	port#addData cont;
	port

let invisibleOutputPort kgraph knode = 
	let port = invisiblePort kgraph knode in
	port#addProperty (PersistentEntry.createPortEast ());
	port

let invisibleInputPort kgraph knode =
	let port = invisiblePort kgraph knode in
	port#addProperty (PersistentEntry.createPortWest ());
	port

let invisibleControlPort kgraph knode = 
	let port = invisiblePort kgraph knode in
	port#addProperty (PersistentEntry.createPortNorth ());
	port

let visibleControlPort ?(ofs=0.0) kgraph node = 
	let port = new kport kgraph in
	port#setHeight 5.0;
	port#setWidth 5.0;
	port#addProperty (PersistentEntry.borderOffset ofs);
	let color = create_color 0 0 0 in
	let cont = new containerRendering in
	cont#addStyle (create_style (Background (create_coloring color)));
	port#setNode node;
	port#addData cont;
	port#addProperty (PersistentEntry.createPortNorth ());
	port	


let notOutputPort kgraph knode = 
	let port = new kport kgraph in
	port#setWidth 5.0;
	port#setHeight 5.0;
	port#addProperty (PersistentEntry.createPortEast ());
	port#setNode knode;
	let cont = new containerRendering in
	cont#setContainer Ellipse;
	cont#addStyle (create_style (LineWidth 1.3));
	cont#addStyle (create_style ~on_sel:true (LineWidth 1.5));
	port#addData cont;
	port


let multOpNode ?(no=false) kgraph node n =
	let rec create n = 
		match n with
		| 0 -> []
		| _ -> (invisibleInputPort kgraph node) :: create (n-1)
	in
	let inputs = create n in
	let po = match no with
	| false ->
		invisibleOutputPort kgraph node
	| true ->
		notOutputPort kgraph node
	in
	{node = node ; inputs = inputs ; outputs = [po] ; control = []}

let binopNode ?(no=false) kgraph node = 
	let p1 = invisibleInputPort kgraph node in
	let p2 = invisibleInputPort kgraph node in
	let po = match no with
	| false -> 
		invisibleOutputPort kgraph node
	| true ->
		notOutputPort kgraph node
	in
	{node = node ; inputs = [p1;p2] ; outputs = [po] ; control = []}

let unopNode ?(no=false) kgraph node = 
	let p1 = invisibleInputPort kgraph node in
	let po = match no with
	| false ->
		invisibleOutputPort kgraph node
	| true ->
		notOutputPort kgraph node
	in
	{node = node ; inputs = [p1] ; outputs = [po] ; control = []}

let resetPortsSurrounding node = 
	node#addProperty (PersistentEntry.create_property "org.eclipse.elk.spacing.individual" "org.eclipse.elk.spacing.portsSurrounding:[top=0.0,left=0.0,bottom=0.0,right=0.0]")

let addPortSpace node = 
	node#addProperty (PersistentEntry.addPortSpace "12.0" "0.0" "10.0" "10.0")

let simpleOpNode ?(ho_mar=0.0) ?(order=false) kgraph text relX relY= 
	let node = defaultNode ~order:order kgraph in
	let cont = simpleOpContWtT () in
	cont#addContainerRendering (simpleText ~ho_mar:ho_mar text relX relY);
	node#addData cont;
	resetPortsSurrounding node;
	node


let simpleXorNode kgraph = 
	binopNode kgraph (simpleOpNode kgraph "=1" 0.5 0.6)

let simpleAndNode kgraph = 
	binopNode kgraph (simpleOpNode kgraph "&amp;" 0.5 0.6)

let simpleOrNode kgraph = 
	binopNode kgraph (simpleOpNode kgraph "&#x2265;1" 0.5 0.6)

let simpleSelectNode kgraph i = 
	unopNode kgraph (simpleOpNode ~ho_mar:2.0 kgraph ("["^i^"]") 0.5 0.5)

let simpleSliceNode kgraph i j = 
	unopNode kgraph (simpleOpNode ~ho_mar:2.0 kgraph ("["^i^"."^j^"]") 0.5 0.5)

let simpleConcatNode kgraph =
	binopNode kgraph (simpleOpNode ~order:true kgraph "." 0.5 0.5)

let rectangleAsPolygon () = 
	let c1 = Point.create_coord Top in
	let c2 = Point.create_coord Bottom in
	let c3 = Point.create_coord Left in
	let c4 = Point.create_coord Right in
	
	let p1 = Point.create_point c3 c1 in
	let p2 = Point.create_point c3 c2 in
	let p3 = Point.create_point c4 c2 in
	let p4 = Point.create_point c4 c1 in
	
	Polygon [p1;p2;p3;p4;p1]

let decoratorNot () = 
	let cont = new containerRendering in
	cont#setContainer Ellipse;
	cont#addStyle (create_style ~on_sel:true (LineWidth 2.0));
	cont#addStyle (create_style (LineWidth 1.3));
	cont#addStyle (create_style (Shadow (2.0,2.0)));
	let color = create_color 0 0 0 in
	cont#addStyle (create_style (Background (create_coloring color)));

	let pd = new DecoratorPlacementData.decoratorPlacementData in
	pd#setYOffset (-2.5);
	pd#setRelative 0.625;
	pd#setHeight 5.0;
	pd#setWidth 5.0;
	
	cont#setPlacement (Decorator pd);

	cont
let simpleNandNode kgraph =
	let node = simpleOpNode kgraph "&amp;" 0.5 0.6 in

	(*node#setContainer (rectangleAsPolygon ());fulladder
	node#addData (decoratorNot ());
*)	binopNode ~no:true kgraph node

let triangle () = 
	let c1 = Point.create_coord Left in
	let c2 = Point.create_coord Bottom in
	let c3 = Point.create_coord Right in
	let c4 = Point.create_coord ~pos_val:(Rel 0.5) Left in
	let c5 = Point.create_coord ~pos_val:(Rel 0.7) Bottom in

	let p1 = Point.create_point c1 c2 in
	let p2 = Point.create_point c3 c2 in
	let p3 = Point.create_point c4 c5 in

	Polygon [p1;p2;p3;p1]

let simpleRegNode kgraph =
	let node = defaultNode kgraph in
	resetPortsSurrounding node;
	let cont = simpleOpContWtT () in
	cont#setContainer (rectangleAsPolygon ());

	let dim = 7.5 in
	let pd = new DecoratorPlacementData.decoratorPlacementData in
	pd#setHeight dim;
	pd#setWidth dim;
	pd#setRelative 0.375;
	pd#setXOffset (-.(dim/.2.));
	pd#setYOffset (-.dim);

	let d = new containerRendering in
	d#setContainer (triangle ());
	d#addStyle (create_style ~on_sel:true (LineWidth 2.0));
	d#addStyle (create_style (LineWidth 1.3));
	d#setPlacement (Decorator pd);
	cont#addContainerRendering d;
	node#addData cont;
	unopNode kgraph node

let simpleBufferNode kgraph = 
	unopNode kgraph (simpleOpNode kgraph "1" 0.5 0.6)

let simpleNotNode kgraph = 
	unopNode ~no:true kgraph (simpleOpNode kgraph "1" 0.5 0.6)


(*	let node = simpleBufferNode kgraph in
	node#setContainer (rectangleAsPolygon ());

	node#addData (decoratorNot ());
	node
*)

let muxContainer () = 
	let c1 = Point.create_coord Left in
	let c2 = Point.create_coord Top in
	let c3 = Point.create_coord Bottom in
	let c4 = Point.create_coord Right in
	let c5 = Point.create_coord ~pos_val:(Rel 0.2) Bottom in
	let c6 = Point.create_coord ~pos_val:(Rel 0.2) Top in
	
	let p1 = Point.create_point c1 c2 in
	let p2 = Point.create_point c1 c3 in
	let p3 = Point.create_point c4 c5 in
	let p4 = Point.create_point c4 c6 in

	Polygon [p1;p2;p3;p4;p1]	

let simpleMuxShape () = 
	let cont = simpleOpContWtT () in
	cont#setContainer (muxContainer ());
	cont

let simpleMuxNode kgraph = 
	let node = defaultNode kgraph in
	let cont = simpleMuxShape () in
	node#setHeight 30.0;
	node#addData cont;
	node#addProperty (PersistentEntry.addPortSpace "0.0" "0.0" "10.0" "10.0");
	(*resetPortsSurrounding node;	*)
	node#addProperty (PersistentEntry.nodeSize "[NODE_LABELS, PORTS, PORT_LABELS, MINIMUM_SIZE]");
	
	let op = binopNode kgraph node in
	let p1 = visibleControlPort ~ofs:(-3.5) kgraph node in
	List.iteri (fun i port ->
		let label = new label in
		label#setText (string_of_int i);
		port#addLabel label) op.inputs;
	{op with control = [p1]}

let simpleCondNode kgraph n_cond = 
	let cont = simpleMuxShape () in
	let node = defaultNode kgraph in
	node#setHeight 40.0;
	node#addData cont;
	node#addProperty (PersistentEntry.nodeSize "[NODE_LABELS, PORTS, PORT_LABELS, MINIMUM_SIZE]");
	node#addProperty (PersistentEntry.addPortSpace "0.0" "0.0" "10.0" "10.0");
	
	(*resetPortsSurrounding node;
	*)
	let op = multOpNode kgraph node (n_cond + 1) in
	let control_ports = 
		let rec create n = match n with | 0 -> []
		| _ ->
			let real = n_cond - n in
			let delta = if real >= 10 then
				8.0 -. 0.02 *. (float_of_int n_cond)
			else 8.0 -. 0.04 *. (float_of_int n_cond)
			in
			let ofs = if real >= 10 then 0.0 else 4.0 in
			(visibleControlPort ~ofs:(-.ofs -. delta *. (float_of_int (n_cond - n))) kgraph node) :: (create (n-1)) in
		create n_cond
	in
	List.iteri (fun i port ->
		let label = new label in
		label#setText (string_of_int (i+1));
		port#addLabel label) control_ports;
	List.iteri (fun i port ->
		let label = new label in
		label#setText (if i = (n_cond) then "else" else string_of_int (i+1));
		port#addLabel label) op.inputs;
	{op with control = control_ports}

let simpleMatchNode kgraph constr_list = 
	let cont = simpleMuxShape () in
	let node = defaultNode kgraph in
	node#setHeight 40.0;
	node#addData cont;
	node#addProperty (PersistentEntry.nodeSize "[NODE_LABELS, PORTS, PORT_LABELS, MINIMUM_SIZE]");
	node#addProperty (PersistentEntry.addPortSpace "0.0" "0.0" "10.0" "10.0");
	
	(*resetPortsSurrounding node;
	*)
	let op = multOpNode kgraph node (List.length constr_list) in
	let control_port = visibleControlPort ~ofs:(-.3.0 -. 3.0 *. (float_of_int (List.length constr_list))) kgraph node in
	List.iter2 (fun port name ->
		let label = new label in
		label#setText name;
		port#addLabel label)
	op.inputs constr_list;
	{op with control = [control_port]}

let simpleTupleShape () = 
	(*let c1 = create_coord Top in
	let c2 = create_coord Bottom in
	let c3 = create_coord Left in
	let p1 = create_point c3 c1 in
	let p2 = create_point c3 c2 in
	*)
	let c = new containerRendering in
	(*c#setContainer (PolyLine [p1;p2]);*)
	c#addStyle (create_style (LineWidth 5.1));
	c

let simpleTupleNode kgraph n = 
	let node = defaultNode kgraph in
	node#setWidth 0.1;
	let cont = simpleTupleShape () in
	node#addData cont;
	resetPortsSurrounding node;
	
	multOpNode kgraph node n

let simpleUnTupleNode kgraph n = 
	let node = defaultNode kgraph in
	node#setWidth 0.1;
	let cont = simpleTupleShape () in
	node#addData cont;
	resetPortsSurrounding node;
	
	let input = invisibleInputPort kgraph node in
	let rec create n = 
		match n with
		| 0 -> []
		| _ -> (invisibleOutputPort kgraph node) :: (create (n-1))
	in
	{node = node ; inputs = [input] ; outputs = create n; control = []}

let simpleConstNode kgraph text = 
	let c1 = Point.create_coord Left in
	let c2 = Point.create_coord Top in
	let c3 = Point.create_coord Bottom in
	let c4 = Point.create_coord ~pos_val:(Abs 8.0) Right in
	let c5 = Point.create_coord  Right in
	let c6 = Point.create_coord ~pos_val:(Rel 0.5) Top in
	
	let p1 = Point.create_point c1 c2 in
	let p2 = Point.create_point c1 c3 in
	let p3 = Point.create_point c4 c3 in
	let p4 = Point.create_point c5 c6 in
	let p5 = Point.create_point c4 c2 in
	
	let d = Polygon [p1;p2;p3;p4;p5;p1] in

	let node = defaultNode ~part:"0" kgraph in
	resetPortsSurrounding node;
	
	let cont = simpleOpContWtT () in
	cont#setContainer d;
	cont#addContainerRendering (simpleText ~ho_mar:(5.0) text 0.5 0.5);
	node#addData cont;	
	let p = invisibleOutputPort kgraph node in
	{node = node ; inputs =[] ; outputs = [p] ; control = []}

let simpleSinkNode kgraph text = 
	let c1 = Point.create_coord Left in
	let c2 = Point.create_coord ~pos_val:(Rel 0.5) Top in
	let c3 = Point.create_coord Bottom in
	let c4 = Point.create_coord ~pos_val:(Abs 8.0) Left in
	let c5 = Point.create_coord  Right in
	let c6 = Point.create_coord Top in
	
	let p1 = Point.create_point c1 c2 in
	let p2 = Point.create_point c4 c3 in
	let p3 = Point.create_point c5 c3 in
	let p4 = Point.create_point c5 c6 in
	let p5 = Point.create_point c4 c6 in
	
	let d = Polygon [p1;p2;p3;p4;p5;p1] in

	let cont = simpleOpContWtT () in
	let node = defaultNode ~part:"2" kgraph in
	cont#setContainer d;
	cont#addContainerRendering (simpleText ~ho_mar:(5.0) text 0.5 0.5);
	node#addData cont;
	resetPortsSurrounding node;
	
	let p = invisibleInputPort kgraph node in
	{node = node ; inputs =[p] ; outputs = [] ; control = []}


let simpleFbyNode kgraph =
	let op = unopNode kgraph (simpleOpNode ~ho_mar:5.0 kgraph "fby" 0.5 0.5) in
	let c = visibleControlPort kgraph op.node in
	{op with control = [c]}

let createPort kgraph node text = 
	let port = new kport kgraph in
	port#setHeight 5.0;
	port#setWidth 5.0;
	let color = create_color 0 0 0 in
	let cont = new containerRendering in
	cont#addStyle (create_style (Background (create_coloring color)));
	port#setNode node;
	port#addData cont;
	let l = new label in
	l#setText text;
	port#addLabel l;
	port

let functionTitle ?(ho_mar=2.0) name =
	let cont = new containerRendering in
	cont#setContainer (Text name);
	cont#addStyle (create_style ~on_sel:true Bold);
	cont#addStyle (create_style (FontSize 13));
	let p = new PointPlacementData.pointPlacementData in
	let c1 = create_coord ~pos_val:(Rel 0.5) Left in
	let c2 = create_coord ~pos_val:(Abs 15.0) Top in
	p#setRefPoint (create_point c1 c2);
	p#setVerticalAlignment CENTER;
	p#setHorizontalAlignment CENTER;
	p#setHorizontalMargin ho_mar;
	
	cont#setPlacement (Point p);
	cont#addProperty (PersistentEntry.create_property "klighd.isNodeTitle" "true");
	cont


let layered_color red green blue layer = 
	let layer = if layer >= 5 then 5 else layer in
	create_color (max 0 (red - 10 * layer)) (max 0 (green - 10 * layer)) (max 0 (blue - 10 * layer))

let aut_color layer = 
	layered_color 240 240 255 0

let fct_color layer = 
	layered_color 224 224 242 layer

let match_color layer = 
	layered_color 200 200 242 layer

let function_node ?(aut=false) ?(m=false) kgraph name inputs outputs layer = 
	let main_node= defaultNode kgraph in
	let cont = new containerRendering in
	let ca = new data in
	ca#setPlacement (place ());
	cont#setChildArea ca;
	cont#addAction (Actions.create_actionCollapse ());
	let background = 
		if not m then if aut then aut_color layer 
		else fct_color layer
		else match_color layer 
	in

	addPortSpace main_node;
	
	cont#addStyle (create_style (Background (create_coloring background)));
	if m then cont#addStyle (create_style (LineStyle DASH));
	cont#addContainerRendering (functionTitle name);
	cont#addStyle (create_style (Shadow (4.0 , 4.0)));
	main_node#addData cont;
	
	main_node#addProperty (PersistentEntry.expand (string_of_bool (layer=0)));
	main_node#addProperty (PersistentEntry.nodeSize "[NODE_LABELS, PORTS, PORT_LABELS, MINIMUM_SIZE]");
	main_node#addProperty (PersistentEntry.activatePartition ());
	if layer > 0 then main_node#addProperty (PersistentEntry.portLabelPlacement "[INSIDE, NEXT_TO_PORT_IF_POSSIBLE]");
	let inputs = List.map (fun input ->
		let port = createPort kgraph main_node input in 
		port#addProperty (PersistentEntry.createPortWest ());
		port
		) inputs in
	
	let outputs = List.map (fun output ->
		let port = createPort kgraph main_node output in
		port#addProperty (PersistentEntry.createPortEast ());
		port
		) outputs in
	{node = main_node ; inputs = inputs ; outputs = outputs ; control = []}

let functionReset ?(m=false) kgraph name inputs outputs layer = 
	let op = function_node ~m:m kgraph name inputs outputs layer in
	{op with control = [visibleControlPort kgraph op.node]}

let addReset kgraph (opNode : op_node) = 
	{opNode with control = [visibleControlPort kgraph opNode.node]}


let state_color layer =
	layered_color 242 224 224 layer

let stateNode ?(init=false) kgraph name layer = 
	let node = defaultStateNode ~part:(if init then "0" else "1") kgraph in
	node#addProperty (PersistentEntry.activatePartition ());
	let cont = new containerRendering in
	let ca = new data in
	ca#setPlacement (place ());
	cont#setChildArea ca;
	cont#addAction (Actions.create_actionCollapse ());

	addPortSpace node;
	

	let color = state_color layer in

	cont#addStyle (create_style (Background (create_coloring color)));
	cont#addStyle (create_style (LineWidth (if init then 3.0 else 1.0)));
	cont#addStyle (create_style ~on_sel:true (LineWidth (if init then 4.5 else 1.5)));
	cont#addContainerRendering (functionTitle ~ho_mar:6.0 name);
		
	node#addProperty (PersistentEntry.expand (string_of_bool (layer=0)));
	node#addProperty (PersistentEntry.nodeSize "[NODE_LABELS, PORTS, PORT_LABELS, MINIMUM_SIZE]");

	cont#setContainer (RoundRect (10.0 , 10.0));
	cont#addStyle (create_style (Shadow (4.0,4.0)));
	node#addData cont;
	node


let sync_color layer = 
	layered_color 210 210 200 layer

let terminalSyncNode kgraph = 
	let node = defaultStateNode ~part:"2" kgraph in
	node#setWidth 40.0;
	node#setHeight 40.0;
	let inside_cont = new containerRendering in
	let color = sync_color 0 in
	inside_cont#addStyle (create_style (Background (create_coloring color)));
	inside_cont#setContainer (RoundRect (15.0 , 15.0));
	inside_cont#setPlacement (place ~top:5.0 ~right:5.0 ~left:5.0 ~bottom:5.0 ());	
	let cont = new containerRendering in
	let color = create_color 255 255 255 in
	cont#addStyle (create_style (Background (create_coloring color)));
	cont#setContainer (RoundRect (20.0,20.0));
	cont#addContainerRendering inside_cont;
	cont#addStyle (create_style (Shadow (4.0,4.0)));
	node#addData cont;
	node

let simpleSyncNode ?(init=false) kgraph =
	let node = defaultStateNode ~part:(if init then "0" else "1") kgraph in
	node#setWidth 30.0;
	node#setHeight 30.0;
	node#addProperty (PersistentEntry.activatePartition ());
	let cont = new containerRendering in
	let ca = new data in
	ca#setPlacement (place ());
	cont#setChildArea ca;
	cont#addAction (Actions.create_actionCollapse ());
	
	addPortSpace node;

	let color = sync_color 0 in
	cont#addStyle (create_style (Background (create_coloring color)));
	cont#addStyle (create_style (LineWidth (if init then 3.0 else 1.0)));
	cont#addStyle (create_style ~on_sel:true (LineWidth (if init then 4.5 else 1.5)));
	cont#addStyle (create_style (Shadow (4.0,4.0)));	
	node#addProperty (PersistentEntry.expand "false");
	node#addProperty (PersistentEntry.nodeSize "[NODE_LABELS, PORTS, PORT_LABELS, MINIMUM_SIZE]");

	cont#setContainer (RoundRect (10.0 , 10.0));
	node#addData cont;
	node

let simpleSeqBlockNode kgraph name = 
	let node = defaultStateNode kgraph in
	node#setWidth 40.0;
	node#setHeight 30.0;
	let cont = new containerRendering in
	let ca = new data in
	ca#setPlacement (place ~top:20.0 ());
	cont#setChildArea ca;
	cont#addAction (Actions.create_actionCollapse ());
	let background = 
		fct_color 0
	in

	cont#addStyle (create_style (Background (create_coloring background)));
	cont#addStyle (create_style (LineStyle DASH));
	cont#addContainerRendering (functionTitle name);
	cont#addStyle (create_style (Shadow (4.0 , 4.0)));
	node#addData cont;
	
	node#addProperty (PersistentEntry.expand "false");
	
	node


let simpleBubleNode ?(init=false) kgraph = 
	let node = defaultStateNode ~part:(if init then "0" else "1") kgraph in
	let cont = new containerRendering in
	let color = create_color 0 0 0 in
	cont#addStyle (create_style (Background (create_coloring color)));
	cont#addStyle (create_style (LineWidth 4.0)); 
	cont#addStyle (create_style ~on_sel:true (LineWidth 6.0));
	cont#addStyle (create_style (Shadow (4.0,4.0)));
	if init then begin
		node#setWidth 40.0;
		node#setHeight 40.0;
	end else begin
		node#setWidth 30.0;
		node#setHeight 30.0;
	end;
	cont#setContainer Ellipse;
	node#addData cont;
	node


let ramNode kgraph = 
	let node = simpleOpNode ~order:true kgraph "ram" 0.5 0.5 in
	let opNode = multOpNode kgraph node 4 in
	opNode

let romNode kgraph = 
	let node = simpleOpNode ~order:true kgraph "rom" 0.5 0.5 in
	multOpNode kgraph node 1

let junction mult =
	let cont = new containerRendering in
	cont#setContainer Ellipse;
	let color = create_color 0 0 0 in
	cont#addStyle (create_style (Background (create_coloring color)));
	let pd = new PointPlacementData.pointPlacementData in
	pd#setVerticalAlignment CENTER;
	pd#setHorizontalAlignment CENTER;
	let size = if mult then 5.0 else 4.0 in
	pd#setMinWidth size;
	pd#setMinHeight size;
	cont#setPlacement (Point pd);
	cont


let arrow_decorator ?(h=false) alone = 
	let place = new DecoratorPlacementData.decoratorPlacementData in
	if alone then 
		place#setAbsolute (-2.0)
	else if h then place#setAbsolute (-15.0) else place#setAbsolute (-10.0);
	place#setXOffset (-10.0);
	place#setYOffset (-4.0);
	place#setRotateWithLine false;
	place#setWidth 12.0;
	place#setHeight 8.0;
	place#setRelative 1.0;

	let c1 = Point.create_coord Right in
	let c2 = Point.create_coord ~pos_val:(Rel 0.5) Bottom in

	let p1 = Point.create_point c1 c2 in

	let c3 = Point.create_coord Left in
	let c4 = Point.create_coord Bottom in
	let p2 = Point.create_point c3 c4 in

	let c5 = Point.create_coord ~pos_val:(Rel 0.4) Left in
	let c6 = Point.create_coord ~pos_val:(Rel 0.5) Top in
	let p3 = Point.create_point c5 c6 in

	let c7 = Point.create_coord Left in
	let c8 = Point.create_coord Top in
	let p4 = Point.create_point c7 c8 in

	let cont = new containerRendering in
	cont#setContainer (Polygon [p4;p3;p2;p1]);
	cont#setPlacement (Decorator place);
	cont#addStyle (create_style JoinRound);
	cont#addStyle (create_style (LineWidth 1.85));
	cont#addStyle (create_style (Background (create_coloring (create_color 0 0 0))));
	cont#addStyle (create_style ~on_sel:true (LineWidth 3.0));
	let color = create_color 100 100 255 in
	cont#addStyle (create_style ~on_sel:true (Background (create_coloring color)));
	cont#addStyle (create_style ~on_sel:true (Foreground (create_coloring color)));
	cont

let red_dot st = 
	let place = new DecoratorPlacementData.decoratorPlacementData in
	if st then 
		place#setAbsolute (4.0)
	else
		place#setAbsolute (-4.0);
	place#setXOffset (-5.0);
	place#setYOffset (-5.0);
	place#setRotateWithLine true;
	place#setWidth 10.0;
	place#setHeight 10.0;
	if st then place#setRelative 0.0
	else place#setRelative 1.0;

	let cont = new containerRendering in
	cont#setContainer Ellipse;
	cont#setPlacement (Decorator place);
	let color = create_color 255 0 0 in
	cont#addStyle (create_style (Background (create_coloring color)));	
	cont


let arrow_red_dot () = 
	let place = new DecoratorPlacementData.decoratorPlacementData in
	place#setXOffset (-20.0);
	place#setYOffset (-5.0);
	place#setRotateWithLine true;
	place#setWidth 22.0;
	place#setHeight 10.0;
	place#setRelative 1.0;
	place#setAbsolute (-1.0);

	let cont = new containerRendering in
	cont#setContainer Rect;
	cont#addStyle (create_style Invisibility);
	cont#setPlacement (Decorator place);
	
	let dot = red_dot false in 
	dot#setPlacement (absolutePointPlacement ~minW:10.0 ~minH:10.0 17. 5.);
	cont#addContainerRendering dot;
	
	let arrow = arrow_decorator true in
	arrow#setPlacement (absolutePointPlacement ~minW:12.0 ~minH:8.0 6. 5.);
	cont#addContainerRendering arrow;
	cont


let history_dot () = 
	let place = new DecoratorPlacementData.decoratorPlacementData in
	place#setAbsolute (-6.5);
	place#setXOffset (-7.5);
	place#setYOffset (-7.5);
	place#setWidth 15.0;
	place#setHeight 15.0;
	place#setRelative 1.0;

	let h = simpleText ~s:7 "H*" 0.5 0.5 in

	let cont = new containerRendering in
	cont#setContainer Ellipse;
	cont#setPlacement (Decorator place);
	cont#addContainerRendering (h);
	let color = create_color 0 200 0 in
	cont#addStyle (create_style (Background (create_coloring color)));
	cont

let arrow_history () = 
	let place = new DecoratorPlacementData.decoratorPlacementData in
	place#setXOffset (-25.0);
	place#setYOffset (-7.5);
	place#setRotateWithLine true;
	place#setWidth 27.0;
	place#setHeight 15.0;
	place#setRelative 1.0;
	place#setAbsolute (-1.0);

	let cont = new containerRendering in
	cont#setContainer Rect;
	cont#addStyle (create_style Invisibility);
	cont#setPlacement (Decorator place);

	let dot = history_dot () in
	dot#setPlacement (absolutePointPlacement ~minW:15.0 ~minH:15.0 19.5 7.5);
	cont#addContainerRendering dot;

	let arrow = arrow_decorator true in
	arrow#setPlacement (absolutePointPlacement ~minW:12.0 ~minH:8.0 6. 7.5);
	cont#addContainerRendering arrow;
	cont

let green_triangle st = 
	let place = new DecoratorPlacementData.decoratorPlacementData in
	if st then
		place#setAbsolute (4.0)
	else
		place#setAbsolute (-4.0);
	place#setXOffset (-5.0);
	place#setYOffset (-5.0);
	place#setRotateWithLine true;
	place#setWidth 10.0;
	place#setHeight 10.0;
	if st then place#setRelative 0.0
	else place#setRelative 1.0;
	
	let c1 = Point.create_coord Left in
	let c2 = Point.create_coord Top in
	let c3 = Point.create_coord Bottom in
	let c4 = Point.create_coord Right in
	let c5 = Point.create_coord ~pos_val:(Rel 0.5) Top in
	let p1 = Point.create_point c1 c2 in
	let p2 = Point.create_point c1 c3 in
	let p3 = Point.create_point c4 c5 in

	let cont = new containerRendering in
	cont#setContainer (Polygon [p1;p2;p3;p1]);
	let color = create_color 100 255 100 in
	cont#addStyle (create_style (Background (create_coloring color)));
	cont#setPlacement (Decorator place);
	cont

let seq_edge ?(half=false) ?(sourcePort=None) ?(targetPort=None) kgraph source target name = 
	let edge = new kedge kgraph in
	edge#setSource source;
	edge#setTarget target;
	begin match sourcePort with
	| None -> ()
	| Some port -> edge#setSourcePort port
	end;
	begin match targetPort with
	| None -> ()
	| Some port -> edge#setTargetPort port
	end;
	
	let cont = new containerRendering in
	cont#setContainer (RoundPolyline (4.0, []));
	cont#addStyle (create_style (LineWidth 2.));
	cont#addStyle (create_style ~on_sel:true (LineWidth 3.));
	let color = create_color 100 100 255 in
	cont#addStyle (create_style ~on_sel:true (Foreground (create_coloring color)));
	if (not half) then cont#addContainerRendering (arrow_decorator true);
	edge#addData cont;
	let label = new label in
	label#setText name;
	label#addProperty (PersistentEntry.edgeLabelPlacement "CENTER");
	edge#addLabel label
	

let automaton_edge kgraph source target name reset beginning = 
	let edge = new kedge kgraph in
	edge#setSource source;
	edge#setTarget target;
	
	let cont = new containerRendering in
	cont#setContainer Spline;
	cont#addStyle (create_style (LineWidth 2.));
	cont#addStyle (create_style ~on_sel:true (LineWidth 3.));
	let color = create_color 100 100 255 in
	cont#addStyle (create_style ~on_sel:true (Foreground (create_coloring color)));
	if beginning then cont#addContainerRendering (red_dot beginning);
	if (not reset) then
		cont#addContainerRendering (arrow_history ())
	else if beginning then 
		cont#addContainerRendering (arrow_decorator true)
	else
		cont#addContainerRendering (arrow_red_dot ());

	(*cont#addContainerRendering (arrow_decorator ~h:(not reset) (beginning && (reset)));
	if reset then (if (not beginning) then cont#addContainerRendering (red_dot false))
	else cont#addContainerRendering (history_dot ()); *)
	edge#addData cont;
	let label = new label in
	label#setText name;
	label#addProperty (PersistentEntry.edgeLabelPlacement "CENTER");
	edge#addLabel label
	
let new_edge ?(mult=false) kgraph source target = 
	let edge = new kedge kgraph in
	edge#setSource source.node;
	edge#setSourcePort source.port;
	edge#setTarget target.node;
	edge#setTargetPort target.port;
	let cont = new containerRendering in
	cont#setContainer (PolyLine []);
	cont#addStyle (create_style (LineWidth (if mult then 1.7 else 1.0)));
	cont#addStyle (create_style ~on_sel:true (LineWidth (if mult then 2.5 else 1.5 )));
	let color = create_color 100 100 255 in
	cont#addStyle (create_style ~on_sel:true (Foreground (create_coloring color)));
	cont#addJunction (junction mult);
	edge#addData cont;
	List.iter (fun t ->
		let label = new label in
		label#setText t;
		label#addProperty (PersistentEntry.edgeLabelPlacement "TAIL");
		edge#addLabel label
	) source.labels;
	List.iter (fun t ->
		let label = new label in
		label#setText t;
		label#addProperty (PersistentEntry.edgeLabelPlacement "HEAD");
		edge#addLabel label
	) target.labels;
	source.labels <- [];
	target.labels <- [];
	edge

let init_kgraph () = 
	let kgraph = new kgraph in
	
	kgraph#addProperty (PersistentEntry.addPortSpace "10.0" "0.0" "10.0" "10.0");
	kgraph
let main _ = 
	let kgraph = new kgraph in
	
	let _ = simpleOrNode kgraph in
	let _ = simpleAndNode kgraph in
	let _ = simpleXorNode kgraph in
	
	let _ = simpleNandNode kgraph in

	let _ = simpleRegNode kgraph in
	
	let _ = simpleBufferNode kgraph in
	
	let _ = simpleNotNode kgraph in

	let _ = simpleMuxNode kgraph in

	let _ = simpleConstNode kgraph "hiaaaaaao" in

	let _ = simpleSelectNode kgraph "2" in
	let _ = simpleSliceNode kgraph "0" "n" in
	let _ = simpleConcatNode kgraph in

	let _ = function_node kgraph "fulladder" ["a";"b"] ["c";"r";"e"] 0 in
	let _ = function_node kgraph "fulladder" ["a";"b"] ["c";"r";"e"] 1 in
	let _ = function_node kgraph "fulladder" ["a";"b"] ["c";"r";"e"] 2 in
	let _ = function_node kgraph "fulladder" ["a";"b"] ["c";"r";"e"] 3 in
	let _ = function_node kgraph "fulladder" ["a";"b"] ["c";"r";"e"] 4 in
	let _ = function_node kgraph "fulladder" ["a";"b"] ["c";"r";"e"] 5 in
	let _ = function_node kgraph "fulladder" ["a";"b"] ["c";"r";"e"] 15 in


	let main = function_node kgraph "main" [] [] 0 in

	let cond = simpleCondNode kgraph 3 in
	let a = simpleCondNode kgraph 4 in
	cond.node#setParent main.node;
	a.node#setParent main.node;

	let matchNode = simpleMatchNode kgraph ["A";"B";"C";"else"] in
	matchNode.node#setParent main.node;
	let matchNode2 = simpleMatchNode kgraph ["A";"B";"C";"else";"B";"C";"else";"B";"C";"else";"B";"C";"else";"B";"C";"else"] in
	matchNode2.node#setParent main.node;
	let matchNode3 = simpleMatchNode kgraph ["A";"B";"C";"else";"B";"C";"else"] in
	matchNode3.node#setParent main.node;



	let _ = simpleFbyNode kgraph in

	let _ = simpleTupleNode kgraph 6 in
	let _ = simpleUnTupleNode kgraph 6 in

	let _ = simpleSinkNode kgraph "ala" in

	let _ = stateNode kgraph "oo" 0 in
	
	let _ = terminalSyncNode kgraph in

	let oc = open_out "basic.kgx" in

	let ff = Format.formatter_of_out_channel oc in
	Kgraph_printer.graph_to_kgx ff kgraph

