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


type return_type = {
	node : knode;
	port : kport;
}

let place () =
	let p = new AreaPlacementData.areaPlacementData in
	let c1 = create_coord ~pos_val:(Abs 10.0) Left in
	let c2 = create_coord ~pos_val:(Abs 10.0) Top in
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
	node#addProperty (PersistentEntry.constraintPortSide ());
	node


let simpleText ?(ho_mar=0.0) ?(ver_mar) text relX relY = 
	ContainerRendering {container = Text text ; data = [(Style (create_style ~on_sel:true Bold) ) ; PlacementData (centerPlacement ~ho_mar:(Some ho_mar) ~ver_mar:ver_mar relX relY)]} 

let invisiblePort kgraph knode = 
	let port = new kport kgraph in
	port#setNode knode;
	port#setHeight 0.1;
	port#setWidth 0.1;
	port#addData (Style (create_style Invisibility));
	port

let invisibleOutputPort kgraph knode = 
	let port = invisiblePort kgraph knode in
	port#addProperty (PersistentEntry.createPortEast ());
	port

let invisibleInputPort kgraph knode =
	let port = invisiblePort kgraph knode in
	port#addProperty (PersistentEntry.createPortWest ());
	port

let notOutputPort kgraph knode = 
	let port = new kport kgraph in
	port#setWidth 5.0;
	port#setHeight 5.0;
	port#addProperty (PersistentEntry.createPortEast ());
	port#setNode knode;
	port#setContainer Ellipse;
	port#addData (Style (create_style (LineWidth 1.3)));
	port#addData (Style (create_style ~on_sel:true (LineWidth 1.5)));
	port


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

let simpleOpNode ?(ho_mar=0.0) kgraph text relX relY= 
	let node = simpleOpNodeWtT kgraph in
	node#addData (simpleText ~ho_mar:ho_mar text relX relY);
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
	binopNode kgraph (simpleOpNode ~ho_mar:2.0 kgraph ("["^i^"."^j^"]") 0.5 0.5)

let simpleConcatNode kgraph =
	binopNode kgraph (simpleOpNode kgraph "." 0.5 0.5)

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

	let s1 = Style (create_style ~on_sel:true (LineWidth 2.0)) in
	let s2 = Style (create_style (LineWidth 1.3)) in
	let s3 = Style (create_style (Shadow (2.0,2.0))) in
	let color = create_color 0 0 0 in
	let s4 = Style (create_style (Background (create_coloring color))) in

	let pd = new DecoratorPlacementData.decoratorPlacementData in
	pd#setYOffset (-2.5);
	pd#setRelative 0.625;
	pd#setHeight 5.0;
	pd#setWidth 5.0;

	ContainerRendering {container = Ellipse ; data = [s1;s2;s3;s4;(PlacementData (Decorator pd))]}

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
	let node = simpleOpNodeWtT kgraph in
	node#setContainer (rectangleAsPolygon ());

	let dim = 7.5 in
	let pd = new DecoratorPlacementData.decoratorPlacementData in
	pd#setHeight dim;
	pd#setWidth dim;
	pd#setRelative 0.375;
	pd#setXOffset (-.(dim/.2.));
	pd#setYOffset (-.dim);

	let s1 = Style (create_style ~on_sel:true (LineWidth 2.0)) in
	let s2 = Style (create_style (LineWidth 1.3)) in
	
	let d = ContainerRendering {container = triangle () ; data = [s1;s2;PlacementData (Decorator pd)]} in
	node#addData d;
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



let simpleMuxNode kgraph = 
	let node = simpleOpNodeWtT kgraph in
	node#setContainer (muxContainer ());
	node#setHeight 30.0;
	let op = binopNode kgraph node in
	let p1 = invisiblePort kgraph node in
	List.iteri (fun i port ->
		let label = new label in
		label#setText (string_of_int i);
		port#addLabel label) op.inputs;
	{op with control = [p1]}

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

	let node = simpleOpNodeWtT kgraph in
	node#setContainer d;
	node#addData (simpleText ~ho_mar:(5.0) text 0.5 0.5);
	
	let p = invisibleOutputPort kgraph node in
	{node = node ; inputs =[] ; outputs = [p] ; control = []}

let createPort kgraph node text = 
	let port = new kport kgraph in
	port#setNode node;
	let l = new label in
	l#setText text;
	port#addLabel l;
	port

let function_node kgraph name inputs outputs = 
	let main_node= new knode kgraph in
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

let new_edge ?(mult=false) kgraph source target = 
	let edge = new kedge kgraph in
	edge#setSource source.node;
	edge#setSourcePort source.port;
	edge#setTarget target.node;
	edge#setTargetPort target.port;
	edge#setContainer (PolyLine []);
	edge#addData (Style (create_style (LineWidth (if mult then 1.3 else 1.0))));
	edge#addData (Style (create_style ~on_sel:true (LineWidth (if mult then 2.0 else 1.5 ))));
	let color = create_color 0 0 255 in
	edge#addData (Style (create_style ~on_sel:true (Foreground (create_coloring color))));
	edge


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

	let f = function_node kgraph "full" ["a";"b"] ["c";"r";"e"] in
	let oc = open_out "basic.kgx" in

	let ff = Format.formatter_of_out_channel oc in
	Kgraph_printer.graph_to_kgx ff kgraph

