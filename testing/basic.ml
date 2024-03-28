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
}

type env = {
	input_env : endPoint Ast.IdentEnv.t;
	output_env : endPoint Ast.IdentEnv.t;
}
*)

let place () =
	let p = new AreaPlacementData.areaPlacementData in
	let c1 = create_coord ~pos_val:(Abs 10.0) Left in
	let c2 = create_coord ~pos_val:(Abs 15.0) Top in
	p#setTopLeft (create_point c1 c2);
	let c3 = create_coord ~pos_val:(Abs 10.0) Right in
	let c4 = create_coord ~pos_val:(Abs 10.0) Bottom in
	p#setBotRight (create_point c3 c4);
	Area p

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

let defaultNode ?(order=false) kgraph = 
	let node = new knode kgraph in
	node#setWidth 20.0;
	node#setHeight 20.0;	
	node#addProperty (PersistentEntry.portAlignmentNorth "CENTER");
	node#addProperty (PersistentEntry.portAlignmentSouth "CENTER");
	node#addProperty (PersistentEntry.partition "1");
	if order then
		node#addProperty (PersistentEntry.constraintPortOrder ())
	else
		node#addProperty (PersistentEntry.constraintPortSide ());
	node#addProperty (PersistentEntry.allowSwitch ()); 
	node

let defaultStateNode kgraph = 
	let node = new knode kgraph in
	node#setWidth 20.0;
	node#setHeight 20.0;
	node#addProperty (PersistentEntry.partition "1");
	node#addProperty (PersistentEntry.portAlignmentNorth "CENTER");
	node#addProperty (PersistentEntry.portAlignmentSouth "CENTER");
	node#addProperty (PersistentEntry.allowSwitch ()); 
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


let simpleText ?(ho_mar=0.0) ?(ver_mar) text relX relY =
	let c = new containerRendering in
	c#setContainer (Text text);
	c#addStyle (create_style ~on_sel:true Bold);
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

let visibleControlPort kgraph node = 
	let port = new kport kgraph in
	port#setHeight 5.0;
	port#setWidth 5.0;
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

	(*node#setContainer (rectangleAsPolygon ());
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
	resetPortsSurrounding node;	
	node#addProperty (PersistentEntry.nodeSize "[NODE_LABELS, PORTS, PORT_LABELS, MINIMUM_SIZE]");
	
	let op = binopNode kgraph node in
	let p1 = visibleControlPort kgraph node in
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
	resetPortsSurrounding node;
	
	let op = multOpNode kgraph node (n_cond + 1) in
	let control_ports = 
		let rec create n = match n with | 0 -> []
		| _ -> (visibleControlPort kgraph node) :: (create (n-1)) in
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
	resetPortsSurrounding node;
	
	let op = multOpNode kgraph node (List.length constr_list) in
	let control_port = visibleControlPort kgraph node in
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

	let node = defaultNode kgraph in
	node#addProperty (PersistentEntry.partition "0");
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
	let node = defaultNode kgraph in
	node#addProperty (PersistentEntry.partition "2");
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

let functionTitle name =
	let cont = new containerRendering in
	cont#setContainer (Text name);
	cont#addStyle (create_style ~on_sel:true Bold);
	cont#addStyle (create_style (FontSize 11));
	let p = new PointPlacementData.pointPlacementData in
	let c1 = create_coord ~pos_val:(Rel 0.5) Left in
	let c2 = create_coord ~pos_val:(Abs 15.0) Top in
	p#setRefPoint (create_point c1 c2);
	p#setVerticalAlignment CENTER;
	p#setHorizontalAlignment CENTER;
	p#setHorizontalMargin 2.0;
	
	cont#setPlacement (Point p);
	cont#addProperty (PersistentEntry.create_property "klighd.isNodeTitle" "true");
	cont



let function_node ?(state=false) ?(m=false) kgraph name inputs outputs layer = 
	let layer = if layer >= 5 then 5 else layer in
	let main_node= if state then defaultStateNode kgraph else defaultNode kgraph in
	if (state) then
		main_node#addProperty (PersistentEntry.activatePartition ());
	let cont = new containerRendering in
	let ca = new data in
	ca#setPlacement (place ());
	cont#setChildArea ca;
	cont#addAction (Actions.create_actionCollapse ());
	let gray = if not m then create_color (224 - 10 * layer) (224 - 10 * layer) (242 - 10 * layer)
     else create_color (200 - 10 * layer) (200 - 10 * layer) (242 - 10 * layer)
	in

	main_node#addProperty (PersistentEntry.addPortSpace "10.0" "0.0" "0.0" "0.0");
	
	cont#addStyle (create_style (Background (create_coloring gray)));
	if m then cont#addStyle (create_style (LineStyle DASH));
	cont#addContainerRendering (functionTitle name);
	main_node#addData cont;
	
	main_node#addProperty (PersistentEntry.expand (string_of_bool (layer=0)));
	main_node#addProperty (PersistentEntry.nodeSize "[NODE_LABELS, PORTS, PORT_LABELS, MINIMUM_SIZE]");
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

let stateNode ?(init=false) kgraph name layer = 
	let layer = if layer >= 5 then 5 else layer in
	let node = defaultStateNode kgraph in
	node#addProperty (PersistentEntry.activatePartition ());
	let cont = new containerRendering in
	let ca = new data in
	ca#setPlacement (place ());
	cont#setChildArea ca;
	cont#addAction (Actions.create_actionCollapse ());
	
	node#addProperty (PersistentEntry.addPortSpace "10.0" "0.0" "0.0" "0.0");
	

	let color = create_color (242 - 10 * layer) (224 - 10 * layer) (224 - 10 * layer) in

	cont#addStyle (create_style (Background (create_coloring color)));
	cont#addStyle (create_style (LineWidth (if init then 3.0 else 1.0)));
	cont#addStyle (create_style ~on_sel:true (LineWidth (if init then 4.5 else 1.5)));
	cont#addContainerRendering (functionTitle name);
	
	node#addProperty (PersistentEntry.expand (string_of_bool (layer=0)));
	node#addProperty (PersistentEntry.nodeSize "[NODE_LABELS, PORTS, PORT_LABELS, MINIMUM_SIZE]");

	cont#setContainer (RoundRect (10.0 , 10.0));
	node#addData cont;
	node

let simpleSyncNode ?(init=false) kgraph =
	let node = defaultStateNode kgraph in
	node#addProperty (PersistentEntry.activatePartition ());
	let cont = new containerRendering in
	let ca = new data in
	ca#setPlacement (place ());
	cont#setChildArea ca;
	cont#addAction (Actions.create_actionCollapse ());

	node#addProperty (PersistentEntry.addPortSpace "10.0" "0.0" "0.0" "0.0");
	

	let color = create_color 200 200 200 in
	cont#addStyle (create_style (Background (create_coloring color)));
	cont#addStyle (create_style (LineWidth (if init then 3.0 else 1.0)));
	cont#addStyle (create_style ~on_sel:true (LineWidth (if init then 4.5 else 1.5)));
	
	node#addProperty (PersistentEntry.expand "false");
	node#addProperty (PersistentEntry.nodeSize "[NODE_LABELS, PORTS, PORT_LABELS, MINIMUM_SIZE]");

	cont#setContainer (RoundRect (10.0 , 10.0));
	node#addData cont;
	node

let simpleBubleNode kgraph = 
	let node = defaultStateNode kgraph in
	let cont = new containerRendering in
	let color = create_color 0 0 0 in
	cont#addStyle (create_style (Background (create_coloring color)));
	cont#addStyle (create_style (LineWidth 2.0)); 
	cont#addStyle (create_style ~on_sel:true (LineWidth 3.0));
	node#setWidth 5.0;
	node#setHeight 5.0;
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


let arrow_decorator () = 
	let place = new DecoratorPlacementData.decoratorPlacementData in
	place#setAbsolute (-2.0);
	place#setXOffset (-6.0);
	place#setYOffset (-3.0);
	place#setRotateWithLine true;
	place#setWidth 8.0;
	place#setHeight 6.0;
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
	cont#addStyle (create_style (LineWidth 2.0));
	cont

	

let automaton_edge kgraph source target name = 
	let edge = new kedge kgraph in
	edge#setSource source;
	edge#setTarget target;
	
	let cont = new containerRendering in
	cont#setContainer Spline;
	cont#addStyle (create_style (LineWidth 2.0));
	cont#addStyle (create_style ~on_sel:true (LineWidth 3.0));
	let color = create_color 224 242 224 in
	cont#addStyle (create_style ~on_sel:true (Foreground (create_coloring color)));
	(*cont#addContainerRendering (arrow_decorator ());
*)
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
	let color = create_color 0 0 255 in
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

	kgraph#addProperty (PersistentEntry.addPortSpace "10.0" "0.0" "0.0" "0.0");
	kgraph
let main _ = 
	let kgraph = new kgraph in
	
	let orNode = simpleOrNode kgraph in
	let andNode = simpleAndNode kgraph in
	let xorNode = simpleXorNode kgraph in
	
	let nandNode = simpleNandNode kgraph in

	let regNode = simpleRegNode kgraph in
	
	let bufferNode = simpleBufferNode kgraph in
	
	let notNode = simpleNotNode kgraph in

	let muxContainer = simpleMuxNode kgraph in

	let const = simpleConstNode kgraph "hiaaaaaao" in

	let selectNode = simpleSelectNode kgraph "2" in
	let sliceNode = simpleSliceNode kgraph "0" "n" in
	let concatNode = simpleConcatNode kgraph in

	let f = function_node kgraph "fulladder" ["a";"b"] ["c";"r";"e"] 0 in
	let f = function_node kgraph "fulladder" ["a";"b"] ["c";"r";"e"] 1 in
	let f = function_node kgraph "fulladder" ["a";"b"] ["c";"r";"e"] 2 in
	let f = function_node kgraph "fulladder" ["a";"b"] ["c";"r";"e"] 3 in
	let f = function_node kgraph "fulladder" ["a";"b"] ["c";"r";"e"] 4 in
	let f = function_node kgraph "fulladder" ["a";"b"] ["c";"r";"e"] 5 in
	let f = function_node kgraph "fulladder" ["a";"b"] ["c";"r";"e"] 15 in

	let cond = simpleCondNode kgraph 3 in

	let matchNode = simpleMatchNode kgraph ["A";"B";"C";"else"] in

	let fby = simpleFbyNode kgraph in

	let tuple = simpleTupleNode kgraph 6 in
	let untuple = simpleUnTupleNode kgraph 6 in

	let sink = simpleSinkNode kgraph "ala" in

	let _ = stateNode kgraph "oo" 0 in
	let oc = open_out "basic.kgx" in

	let ff = Format.formatter_of_out_channel oc in
	Kgraph_printer.graph_to_kgx ff kgraph

