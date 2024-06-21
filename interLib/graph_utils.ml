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
open Intermediate_graph
let default_informations = new iInformation

let layered_color red green blue layer = 
	let layer = if layer >= 5 then 5 else layer in
	create_color (max 0 (red - 10 * layer)) (max 0 (green - 10 * layer)) (max 0 (blue - 10 * layer))

let aut_color _ = 
	layered_color 240 240 255 0

let fct_color layer = 
	layered_color 224 224 242 layer

let match_color layer = 
	layered_color 200 200 242 layer

let reset_color layer = 
	layered_color 190 200 250 layer

let operation_color () = 
	layered_color 200 200 250 0

let cond_color () = 
	create_color 200 250 225 

let place ?(top=15.0) ?(left=10.0) ?(right=10.0) ?(bottom=10.0) () =
	let p = new AreaPlacementData.areaPlacementData in
	let c1 = create_coord ~pos_val:(Abs left) Left in
	let c2 = create_coord ~pos_val:(Abs top) Top in
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

let opLineColor custom = 
	if custom#getInCycle then create_color 255 20 20 
	else if custom#getDead then
		create_color 100 100 100
	else create_color 0 0 0

let defaultNode ?(layer) ?(order=false) ?(side=true) kgraph = 
	let node = new knode kgraph in
	node#setWidth 20.0;
	node#setHeight 20.0;	
	node#addProperty (PersistentEntry.portAlignmentNorth "CENTER");
	node#addProperty (PersistentEntry.portAlignmentSouth "CENTER");
	node#addProperty (PersistentEntry.separateComponent false);
	begin match layer with
	| Some s -> 
		node#addProperty (PersistentEntry.layerConstraint s);
	| None -> ()
	end;
	if order then
		node#addProperty (PersistentEntry.constraintPortOrder ())
	else if side then
		node#addProperty (PersistentEntry.constraintPortSide ());
	node

let defaultStateNode ?(layer) kgraph = 
	let node = new knode kgraph in
	node#setWidth 20.0;
	node#setHeight 20.0;
	node#addProperty (PersistentEntry.separateComponent false);
	begin match layer with
	| Some s -> node#addProperty (PersistentEntry.layerConstraint s)
	| None -> ()
	end;
	node


let simpleOpContWtT ?(spe_back=None) ?(inv=false) ?(custom=default_informations) () =
	let cont = new containerRendering in
	cont#setContainer Rect;
	cont#addStyle (create_style (LineWidth 1.3));
	cont#addStyle (create_style ~on_sel:true (LineWidth 2.0));
	cont#addStyle (create_style (Foreground (create_coloring (opLineColor custom))));
	if inv then
		cont#addStyle (create_style Invisibility);
	let color = match spe_back with
	| None -> operation_color () 
	| Some c -> c
	in
	cont#addStyle (create_style (Background (create_coloring color)));
	cont#addStyle (create_style (Shadow (2.0,2.0)));
	cont

let simpleOpNodeWtT ?(custom=default_informations) kgraph =
	let node = defaultNode kgraph in
	node#addData (simpleOpContWtT ~custom:custom ());
	node

let functionTitle ?(custom=default_informations) ?(center=false) ?(ho_mar=2.0) name =
	let cont = new containerRendering in
	cont#setContainer (Text name);
	cont#addStyle (create_style ~on_sel:true Bold);
	cont#addStyle (create_style (FontSize 13));
	let p = new PointPlacementData.pointPlacementData in
	let c1 = create_coord ~pos_val:(Rel 0.5) Left in
	let c2 = if center then
		create_coord ~pos_val:(Rel 0.5) Top 
		else create_coord ~pos_val:(Abs 15.0) Top 
	in
	p#setRefPoint (create_point c1 c2);
	p#setVerticalAlignment CENTER;
	p#setHorizontalAlignment CENTER;
	p#setHorizontalMargin ho_mar;
	
	cont#setPlacement (Point p);
	cont#addStyle (create_style (Foreground (create_coloring (opLineColor custom))));

	cont#addProperty (PersistentEntry.create_property "klighd.isNodeTitle" "true");
	cont

let simpleText ?(custom=default_informations) ?(s=11) ?(ho_mar=0.0) ?(ver_mar) text relX relY =
	let c = new containerRendering in
	c#setContainer (Text text);
	c#addStyle (create_style ~on_sel:true Bold);
	c#addStyle (create_style (FontSize s));
	c#addStyle (create_style (Foreground (create_coloring (opLineColor custom))));
	c#setPlacement (centerPlacement ~ho_mar:(Some ho_mar) ~ver_mar:ver_mar relX relY);
	c

let addPortOffset kport ofs = 
	match ofs with
	| None -> ()
	| Some ofs ->
		kport#addProperty (PersistentEntry.borderOffset ofs)

let invisiblePort ?(custom=default_informations) ?(ofs=None) kgraph knode = 
	let port = new kport kgraph in
	port#setNode knode;
	port#setHeight (0.01);
	port#setWidth (0.01);
	let cont = new containerRendering in
	let color = opLineColor custom in
	cont#addStyle (create_style (Background (create_coloring color)));
	cont#addStyle (create_style (Foreground (create_coloring color)));
	port#addData cont;
	begin match ofs with
	| None ->
		addPortOffset port (Some (-0.1));
	| Some _ -> addPortOffset port ofs
	end;
	port

let visiblePort ?(custom=default_informations) ?(ofs=None) kgraph knode = 
	let port = new kport kgraph in
	port#setNode knode;
	port#setHeight 5.0;
	port#setWidth 5.0;
	let color = opLineColor custom in
	let cont = new containerRendering in
	cont#addStyle (create_style (Background (create_coloring color)));
	cont#addStyle (create_style (Foreground (create_coloring color)));
	port#addData cont;
	addPortOffset port ofs;
	port

let invisibleOutputPort ?(custom=default_informations) ?(ofs=None) kgraph knode = 
	let port = invisiblePort ~custom:custom ~ofs:ofs kgraph knode in
	port#addProperty (PersistentEntry.createPortEast ());
	port

let invisibleInputPort ?(custom=default_informations) ?(ofs=None) kgraph knode =
	let port = invisiblePort ~custom:custom ~ofs:ofs kgraph knode in
	port#addProperty (PersistentEntry.createPortWest ());
	port

let invisibleControlPort ?(custom=default_informations) ?(ofs=None) kgraph knode = 
	let port = invisiblePort ~custom:custom ~ofs:ofs kgraph knode in
	port#addProperty (PersistentEntry.createPortNorth ());
	(*port#addProperty (PersistentEntry.allowSwitch ()); *)
	port

let visibleControlPort ?(custom=default_informations) ?(ofs=None) kgraph knode = 
	let port = visiblePort ~custom:custom ~ofs:ofs kgraph knode in	
	port#addProperty (PersistentEntry.createPortNorth ());
	(*port#addProperty (PersistentEntry.allowSwitch ()); *)
	port	

let visibleInputPort ?(custom=default_informations) ?(ofs=None) kgraph knode = 
	let port = visiblePort ~custom:custom ~ofs:ofs kgraph knode in
	port#addProperty (PersistentEntry.createPortWest ());
	port

let visibleOutputPort ?(custom=default_informations) ?(ofs=None) kgraph knode = 
	let port = visiblePort ~custom:custom ~ofs:ofs kgraph knode in
	port#addProperty (PersistentEntry.createPortEast ());
	port

let visibleBotPort ?(custom=default_informations) ?(ofs=None) kgraph knode =
	let port = visiblePort ~custom:custom kgraph knode in
	port#addProperty (PersistentEntry.createPortSouth ());
	port

let invisibleBotPort ?(custom=default_informations) ?(ofs=None) kgraph knode =
	let port = invisiblePort ~custom:custom kgraph ~ofs:ofs knode in
	port#addProperty (PersistentEntry.createPortSouth ());
	port


let notOutputPort ?(custom=default_informations) ?(ofs=None) kgraph knode = 
	let port = new kport kgraph in
	port#setWidth 5.0;
	port#setHeight 5.0;
	port#addProperty (PersistentEntry.createPortEast ());
	port#setNode knode;
	addPortOffset port ofs;
	let cont = new containerRendering in
	cont#setContainer Ellipse;
	cont#addStyle (create_style (LineWidth 1.3));
	cont#addStyle (create_style (Foreground (create_coloring (opLineColor custom))));
	cont#addStyle (create_style ~on_sel:true (LineWidth 1.5));
	port#addData cont;
	port

let questionOutputPort ?(custom=default_informations) ?(ofs=None) kgraph knode = 
	let port = new kport kgraph in
	port#setWidth 10.0;
	port#setHeight 10.0;
	port#addProperty (PersistentEntry.createPortEast ());
	port#setNode knode;
	addPortOffset port ofs;
	let cont = new containerRendering in
	cont#setContainer Rect;
	cont#addStyle (create_style (LineWidth 1.0));
	cont#addStyle (create_style (Foreground (create_coloring (opLineColor custom))));
	cont#addStyle (create_style ~on_sel:true (LineWidth 1.5));
	let back_color = create_color 200 200 250 in
	cont#addStyle (create_style (Background (create_coloring back_color)));
	cont#addContainerRendering (simpleText ~custom:custom ~s:6 "?" 0.5 0.5);
	port#addData cont;
	port

let resetPortsSurrounding node = 
	node#addProperty (PersistentEntry.create_property "org.eclipse.elk.spacing.individual" "org.eclipse.elk.spacing.portsSurrounding:[top=0.0,left=0.0,bottom=0.0,right=0.0]")

let addPortSpace node = 
	node#addProperty (PersistentEntry.addPortSpace "12.0" "0.0" "10.0" "10.0")

let simpleOpNode ?(inv=false) ?(custom=default_informations) ?(center=false) ?(title=false) ?(ho_mar=0.0) ?(order=false) kgraph text relX relY= 
	let node = defaultNode ~order:order kgraph in
	let cont = simpleOpContWtT ~inv:inv ~custom:custom () in
	if not inv then cont#addContainerRendering (if title then functionTitle ~custom:custom ~center:center text else simpleText ~custom:custom ~ho_mar:ho_mar text relX relY);
	node#addData cont;
	resetPortsSurrounding node;
	node


let simpleLinkNode ?(custom=default_informations) kgraph = 
	let node = simpleOpNode ~inv:(not !InterLib_options.do_show_all) ~custom:custom kgraph "L" 0.5 0.5 in
	node#addProperty (PersistentEntry.create_property "org.eclipse.elk.noLayout" (string_of_bool (not !InterLib_options.do_show_all)));
	node

let simpleXorNode ?(custom=default_informations) kgraph = 
	simpleOpNode ~custom:custom kgraph "=1" 0.5 0.6

let simpleAndNode ?(custom=default_informations) kgraph = 
	simpleOpNode ~custom:custom kgraph "&amp;" 0.5 0.6

let simpleOrNode ?(custom=default_informations) kgraph = 
	simpleOpNode ~custom:custom kgraph "&#x2265;1" 0.5 0.6

let simpleSelectNode ?(custom=default_informations) kgraph i = 
	simpleOpNode ~custom:custom ~ho_mar:2.0 kgraph ("["^i^"]") 0.5 0.5

let simpleSliceNode ?(custom=default_informations) kgraph i j = 
	simpleOpNode ~custom:custom ~ho_mar:2.0 kgraph ("["^i^"."^j^"]") 0.5 0.5

let simpleConcatNode ?(custom=default_informations) kgraph =
	simpleOpNode ~custom:custom ~order:true kgraph "." 0.5 0.5

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

let simpleNandNode ?(custom=default_informations) kgraph =
	let node = simpleOpNode ~custom:custom kgraph "&amp;" 0.5 0.6 in
	node

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

let simpleRegNode ?(custom=default_informations) kgraph =
	let node = defaultNode kgraph in
	resetPortsSurrounding node;
	let cont = simpleOpContWtT () in
	cont#setContainer (rectangleAsPolygon ());
	cont#addStyle (create_style (Foreground (create_coloring (opLineColor custom))));
	let dim = 7.5 in
	let pd = new DecoratorPlacementData.decoratorPlacementData in
	pd#setHeight dim;
	pd#setWidth dim;
	pd#setRelative 0.375;
	pd#setXOffset (-.(dim/.2.));
	pd#setYOffset (-.dim);

	let d = new containerRendering in
	d#setContainer (triangle ());
	d#addStyle (create_style (Foreground (create_coloring (opLineColor custom))));
	d#addStyle (create_style ~on_sel:true (LineWidth 2.0));
	d#addStyle (create_style (LineWidth 1.3));
	d#setPlacement (Decorator pd);
	cont#addContainerRendering d;
	node#addData cont;
	node

let simpleBufferNode ?(custom=default_informations) kgraph = 
	simpleOpNode ~custom:custom kgraph "1" 0.5 0.6

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

let simpleMuxShape ?(custom=default_informations) () = 
	let cont = simpleOpContWtT ~custom:custom () in
	cont#setContainer (muxContainer ());
	cont

let simpleMuxNode ?(custom=default_informations) kgraph = 
	let node = defaultNode kgraph in
	let cont = simpleMuxShape ~custom:custom () in
	node#setHeight 30.0;
	node#addData cont;
	resetPortsSurrounding node;	
	node#addProperty (PersistentEntry.nodeSize "[NODE_LABELS, PORTS, PORT_LABELS, MINIMUM_SIZE]");	
	node

let simpleCondNode ?(custom=default_informations) kgraph = 
	let cont = simpleMuxShape ~custom:custom () in
	let node = defaultNode ~order:true kgraph in
	node#setHeight 40.0;
	node#addData cont;
	node#addProperty (PersistentEntry.nodeSize "[NODE_LABELS, PORTS, PORT_LABELS, MINIMUM_SIZE]");
	resetPortsSurrounding node;
	node

let simpleMatchNode ?(custom=default_informations) kgraph = 
	let cont = simpleMuxShape ~custom:custom () in
	let node = defaultNode kgraph in
	node#setHeight 40.0;
	node#addData cont;
	node#addProperty (PersistentEntry.nodeSize "[NODE_LABELS, PORTS, PORT_LABELS, MINIMUM_SIZE]");
	node#addProperty (PersistentEntry.addPortSpace "0.0" "0.0" "10.0" "10.0");
	node

let simpleTupleNode ?(custom=default_informations) kgraph = 
	simpleOpNode ~custom:custom kgraph "()" 0.5 0.5 

let simpleConstNode ?(custom=default_informations) ?(const=true) kgraph text = 
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

	let node = defaultNode ~layer:(if const then "NONE" else "FIRST") kgraph in
	resetPortsSurrounding node;
	
	let cont = simpleOpContWtT ~custom:custom () in
	cont#setContainer d;
	cont#addContainerRendering (simpleText ~custom:custom ~ho_mar:(5.0) text 0.5 0.5);
	node#addData cont;	
	node

let simpleInputVarNode ?(custom=default_informations) kgraph text = 
	simpleConstNode ~custom:custom ~const:false kgraph text

let simpleSinkNode ?(custom=default_informations) ?(used=true) kgraph text = 
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

	let cont = simpleOpContWtT ~custom:custom () in
	let node = defaultNode ~layer:(if used then "LAST" else "NONE") kgraph in
	cont#setContainer d;
	cont#addContainerRendering (simpleText ~custom:custom ~ho_mar:(5.0) text 0.5 0.5);
	node#addData cont;
	resetPortsSurrounding node;
	node

let simpleFbyNode ?(custom=default_informations) kgraph =
	simpleOpNode ~custom:custom ~ho_mar:5.0 kgraph "fby" 0.5 0.5 

let simpleTextNode ?(custom=default_informations) kgraph text = 
	simpleOpNode ~custom:custom ~ho_mar:5.0 kgraph text 0.5 0.5

(* for z *)

let simpleAddNode ?(custom=default_informations) kgraph = 
	simpleOpNode ~custom:custom kgraph "+" 0.5 0.5

let simpleMinusNode ?(custom=default_informations) kgraph = 
	simpleOpNode ~custom:custom ~order:true kgraph "-" 0.5 0.5

let simpleTimesNode ?(custom=default_informations) kgraph = 
	simpleOpNode ~custom:custom kgraph "*" 0.5 0.6

let simpleDivNode ?(custom=default_informations) kgraph = 
	simpleOpNode ~custom:custom ~order:true kgraph "/" 0.5 0.5

let simpleLastNode ?(custom=default_informations) kgraph = 
	simpleOpNode ~custom:custom kgraph ~ho_mar:(5.0) "last" 0.5 0.5


let constrBox ?(custom=default_informations) ?(no_line=false) cont rightDelta high =

	let dim = 5.0 in
	let circle = new containerRendering in
	circle#setContainer Ellipse;
	circle#addStyle (create_style (LineWidth 1.0));
	circle#addStyle (create_style ~on_sel:true (LineWidth 2.0));
	circle#addStyle (create_style (Foreground (create_coloring (opLineColor custom))));
	circle#addStyle (create_style (Background (create_coloring (opLineColor custom))));

	let pd = new PointPlacementData.pointPlacementData in
	pd#setMinHeight dim;
	pd#setMinWidth dim;
	pd#setHorizontalAlignment CENTER;
	pd#setVerticalAlignment CENTER;
	let c1 = Point.create_coord ~pos_val:(Abs (dim *. (rightDelta +.1.5))) Right in
	let c2 = Point.create_coord ~pos_val:(Rel 0.5) Top in
	let point = Point.create_point c1 c2 in
	pd#setRefPoint point;
	circle#setPlacement (Point pd);
	cont#addContainerRendering circle;
	
	
	let quad = new containerRendering in
	quad#setContainer Rect;
	quad#addStyle (create_style (LineWidth 1.0));
	quad#addStyle (create_style (LineStyle DASH));
	quad#addStyle (create_style ~on_sel:true (LineWidth 1.5));
	quad#addStyle (create_style (Foreground (create_coloring (opLineColor custom))));

	let pd = new PointPlacementData.pointPlacementData in
	pd#setMinHeight (dim*.2.);
	pd#setMinWidth (dim*.2.);
	pd#setHorizontalAlignment CENTER;
	pd#setVerticalAlignment CENTER;
	let c1 = Point.create_coord ~pos_val:(Abs (dim *. (rightDelta +. 1.5))) Right in
	let c2 = Point.create_coord ~pos_val:(Rel 0.5) Top in
	let point = Point.create_point c1 c2 in
	pd#setRefPoint point;
	quad#setPlacement (Point pd);
	cont#addContainerRendering quad;
	
	let line = new containerRendering in
	line#addStyle (create_style (LineWidth 1.0));
	line#addStyle (create_style ~on_sel:true (LineWidth 1.7));
	line#addStyle (create_style (Foreground (create_coloring (opLineColor custom))));
	line#addStyle (create_style (Background (create_coloring (opLineColor custom))));

	let c1 = Point.create_coord Left in
	let c2 = Point.create_coord Right in
	let c3 = Point.create_coord Top in
	let c4 = Point.create_coord Bottom in
	if high  then begin
		let p1 = Point.create_point c1 c4 in
		let p2 = Point.create_point c1 c3 in
		line#setContainer (PolyLine [p1;p2]);
	end else begin
		let p1 = Point.create_point c1 c3 in
		let p2 = Point.create_point c2 c3 in
		line#setContainer (PolyLine [p1;p2]);
	end;
	if not no_line then begin
		let pd = new PointPlacementData.pointPlacementData in
		if high then begin 
			pd#setMinHeight (dim*.2.);
			pd#setMinWidth 1.0;
			pd#setHorizontalAlignment LEFT;
			pd#setVerticalAlignment CENTER;	
		end else begin
			pd#setMinHeight 1.0;
			pd#setMinWidth (dim*.2.1);
			pd#setVerticalAlignment TOP;
			pd#setHorizontalAlignment CENTER;
		end;
		let c1 = if high then 
			Point.create_coord ~pos_val:(Abs (dim *. (rightDelta +. 1.5))) Right 
			else Point.create_coord ~pos_val:(Abs (dim *. 1.05)) Right in
		let c2 = if high then
			Point.create_coord ~pos_val:(Rel 0.25) Top 
			else Point.create_coord ~pos_val:(Rel 0.5) Top in
		let point = Point.create_point c1 c2 in
		pd#setRefPoint point;
		line#setPlacement (Point pd);
		cont#addContainerRendering line
	end
(*
let constrPort ?(custom=default_informations) kgraph knode n = 
	let port = new kport kgraph in
	let dim = 5.0 in
	port#setNode knode;
	port#setHeight dim;
	port#setWidth dim;
	let rightDelta = 1.0 +. n *. 3.0 in
	let circle = new containerRendering in
	circle#setContainer Ellipse;
	circle#addStyle (create_style (LineWidth 1.0));
	circle#addStyle (create_style ~on_sel:true (LineWidth 2.0));
	circle#addStyle (create_style (Foreground (create_coloring (opLineColor custom))));
	circle#addStyle (create_style (Background (create_coloring (opLineColor custom))));

	let pd = new PointPlacementData.pointPlacementData in
	pd#setMinHeight dim;
	pd#setMinWidth dim;
	pd#setHorizontalAlignment CENTER;
	pd#setVerticalAlignment CENTER;
	let c1 = Point.create_coord ~pos_val:(Abs (dim *. (rightDelta +.1.5))) Right in
	let c2 = Point.create_coord ~pos_val:(Abs (2. *. dim)) Top in
	let point = Point.create_point c1 c2 in
	pd#setRefPoint point;
	circle#setPlacement (Point pd);
	port#addData circle;
	port#addProperty (PersistentEntry.createPortEast ());
	port
*)

let simpleDeConstrNode ?(custom=default_informations) ?(vert=false) ?(right_ofs=27.35) kgraph name num = 
	let node = defaultNode ~order:true kgraph in
	let cont = simpleOpContWtT ~custom:custom () in
	let rec addBox n = match n with
		| -1 -> ()
		| _ -> 
			constrBox ~custom:custom cont (1.0 +. 3.0 *. (float_of_int n)) (vert || n <> 0);
			addBox (n - 1)
	in
	addBox (num - 1);
	let num = float_of_int num in
	
	let t = simpleText ~s:8 ~custom:custom name 0.5 0.5 in
	t#setPlacement (place ~top:0.0 ~left:5.0 ~right:(1.0 +. 3.0 *.num *. 5.0 +. 5.0) ~bottom:0.0 ());	
	cont#addContainerRendering t;

	node#addData cont;

	node#addProperty (PersistentEntry.create_property "org.eclipse.elk.spacing.individual" 
	("org.eclipse.elk.spacing.portPort:15.0;,;org.eclipse.elk.spacing.portsSurrounding:[top=0.0,left=0.0,bottom=0.0,right="^(string_of_float right_ofs)^"]"));

	node#addProperty (PersistentEntry.create_property "org.eclipse.elk.portAlignment.north" "END");
	node

let simpleConstrNode ?(custom=default_informations) kgraph name num =
	simpleDeConstrNode ~custom:custom ~vert:true ~right_ofs:12.35 kgraph name num


let integrCont ?(custom=default_informations) () = 
	let line = new containerRendering in

	let c1 = Point.create_coord Left in
	let c2 = Point.create_coord Bottom in
	let c3 = Point.create_coord ~pos_val:(Rel 0.5) Left in
	let c4 = Point.create_coord Top in
	let c5 = Point.create_coord Right in

	let p1 = Point.create_point c1 c2 in
	let p2 = Point.create_point c3 c2 in
	let p3 = Point.create_point c3 c4 in
	let p4 = Point.create_point c5 c4 in
	line#setContainer (PolyLine [p1;p2;p3;p4]);

	line#addStyle (create_style (LineWidth 1.5));
	line#addStyle (create_style ~on_sel:true (LineWidth 2.));
	line#addStyle (create_style (Foreground (create_coloring (opLineColor custom))));
	line#addStyle (create_style (Background (create_coloring (opLineColor custom))));

	let pd = new PointPlacementData.pointPlacementData in
	pd#setMinHeight 12.0;
	pd#setMinWidth 25.0;
	pd#setHorizontalAlignment CENTER;
	pd#setVerticalAlignment CENTER;
	let c1 = Point.create_coord ~pos_val:(Rel 0.5) Left in
	let c2 = Point.create_coord ~pos_val:(Abs 10.0) Top in
	let point = Point.create_point c1 c2 in
	pd#setRefPoint point;
	line#setPlacement (Point pd);
	line	


let simpleDerNode ?(custom=default_informations) kgraph init_name =
	let node = defaultNode ~order:true kgraph in
	node#setWidth 30.0;
	node#setHeight 30.0;
	let cont = simpleOpContWtT ~custom:custom () in
	cont#addContainerRendering (integrCont ~custom:custom ());
	let t = simpleText ~s:8 ~custom:custom init_name 0.5 0.5 in
	t#setPlacement (place ~top:15.0 ~left:5.0 ~right:5.0 ~bottom:0.0 ());	
	cont#addContainerRendering t;
	node#addData cont;
	resetPortsSurrounding node;
	node

let testCondCont ?(custom=default_informations) () = 

	let c1 = Point.create_coord Left in
	let c2 = Point.create_coord Top in
	let c3 = Point.create_coord ~pos_val:(Abs 5.0) Left in
	let c4 = Point.create_coord Bottom in
	let c5 = Point.create_coord ~pos_val:(Abs 5.0) Right in
	let c6 = Point.create_coord Right in

	let p1 = Point.create_point c1 c2 in
	let p2 = Point.create_point c3 c4 in
	let p3 = Point.create_point c5 c4 in
	let p4 = Point.create_point c6 c2 in
	let cont = simpleOpContWtT ~spe_back:(Some (cond_color ())) ~custom:custom () in
	cont#setContainer (Polygon [p1;p2;p3;p4;p1]);
	cont

let simpleScondNode ?(custom=default_informations) kgraph title = 
	let node = defaultNode kgraph in
	let cont = testCondCont ~custom:custom () in
	cont#addAction (Actions.create_actionCollapse ());
	node#addData cont;
	resetPortsSurrounding node;
	begin match title with
	| None -> ()
	| Some t -> 
		let tT = functionTitle ~ho_mar:5.0 ~custom:custom t in cont#addContainerRendering tT;
		let ca = new data in
		ca#setPlacement (place ());
		cont#setChildArea ca;
	end;
	node


let simpleInvisibleNode ?(custom=default_informations) kgraph =
	let node = defaultNode ~order:true kgraph in
	
	let cont = simpleOpContWtT ~inv:true ~custom:custom () in
	node#addData cont;
	addPortSpace node;
	
	node

let simplePresentNode ?(custom=default_informations) kgraph = 
	let node = defaultNode kgraph in
	let cont = simpleOpContWtT ~spe_back:(Some (fct_color custom#getLayer)) ~custom:custom () in
	cont#setContainer Rect;
	let tT = functionTitle ~custom:custom "Present" in 
	cont#addContainerRendering tT;
	let ca = new data in
	ca#setPlacement (place ());
	cont#setChildArea ca;
	node#addData cont;
	cont#addAction (Actions.create_actionCollapse ());	
	addPortSpace node;
	node#addProperty (PersistentEntry.nodeSize "[NODE_LABELS, PORTS, PORT_LABELS, MINIMUM_SIZE]");	
	node#addProperty (PersistentEntry.portLabelPlacement "[INSIDE, NEXT_TO_PORT_IF_POSSIBLE]");
	node

let simpleResetDerNode ?(custom=default_informations) kgraph = 
	let node = defaultNode kgraph in
	let cont = simpleOpContWtT ~spe_back:(Some (fct_color custom#getLayer)) ~custom:custom () in
	cont#setContainer Rect;
	let tT = functionTitle ~custom:custom "Reset" in 
	cont#addContainerRendering tT;
	let ca = new data in
	ca#setPlacement (place ());
	cont#setChildArea ca;
	node#addData cont;
	cont#addAction (Actions.create_actionCollapse ());	
	addPortSpace node;
	node#addProperty (PersistentEntry.nodeSize "[NODE_LABELS, PORTS, PORT_LABELS, MINIMUM_SIZE]");	
	node#addProperty (PersistentEntry.portLabelPlacement "[INSIDE, NEXT_TO_PORT_IF_POSSIBLE]");
	node


let simplePeriodNode ?(custom=default_informations) kgraph = 
	simpleOpNode ~custom:custom ~order:true ~ho_mar:5.0 kgraph "Period" 0.5 0.5

let simpleEmitNode ?(custom=default_informations) kgraph name = 
	let node = defaultNode ~order:true kgraph in
	let cont = simpleOpContWtT ~custom:custom () in
	let t = simpleText ~custom:custom "emit" 0.5 0.5 in
	t#setPlacement (place ~top:0.0 ~left:5.0 ~right:5.0 ~bottom:12.0 ());	
	cont#addContainerRendering t;
	let t = simpleText ~custom:custom name 0.5 0.5 in
	t#setPlacement (place ~top:12.0 ~left:5.0 ~right:5.0 ~bottom:0.0 ());
	cont#addContainerRendering t;
	node#addData cont;
	resetPortsSurrounding node;
	node

let simpleUpNode ?(custom=default_informations) kgraph = 
	simpleOpNode ~custom:custom ~ho_mar:5.0 kgraph "Up" 0.5 0.5

let simpleBlanckNode ?(custom=default_informations) kgraph side = 
	let node = defaultNode ~side:side kgraph in
	let cont = simpleOpContWtT ~spe_back:(Some (fct_color custom#getLayer)) ~custom:custom () in
	node#addData cont;
	addPortSpace node;
	cont#addAction (Actions.create_actionCollapse ());
	node#addProperty (PersistentEntry.nodeSize "[NODE_LABELS, PORTS, PORT_LABELS, MINIMUM_SIZE]");	
	node

let simpleMinusGreaterNode ?(custom=default_informations) kgraph = 
	simpleOpNode ~custom:custom ~ho_mar:5.0 kgraph "-&gt;" 0.5 0.5


let simpleNextNode ?(custom=default_informations) kgraph name = 
	let node = defaultNode ~order:true kgraph in
	let cont = simpleOpContWtT ~custom:custom () in
	let t = simpleText ~custom:custom "next" 0.5 0.5 in
	t#setPlacement (place ~top:0.0 ~left:5.0 ~right:5.0 ~bottom:12.0 ());	
	cont#addContainerRendering t;
	let t = simpleText ~custom:custom name 0.5 0.5 in
	t#setPlacement (place ~top:12.0 ~left:5.0 ~right:5.0 ~bottom:0.0 ());
	cont#addContainerRendering t;
	node#addData cont;
	resetPortsSurrounding node;
	node


let simpleInnerRecordNode ?(custom=default_informations) kgraph name = 
	let node = defaultNode kgraph in
	let cont = new containerRendering in
	cont#setContainer Rect;
	cont#addStyle (create_style (LineWidth 0.0));
	cont#addContainerRendering (simpleText ~custom:custom name 0.5 0.5);
	node#addData cont;
	resetPortsSurrounding node;
	node


let simpleRecordNode ?(custom=default_informations) kgraph = 
	let node = defaultNode ~order:true kgraph in	
	let cont = simpleOpContWtT ~custom:custom () in
	node#addProperty (PersistentEntry.create_property "org.eclipse.elk.direction" "UP");
	node#addData cont;
	node

type direction = North | South | East | West

let bublePort ?(custom=default_informations) ?(ofs=None) ?(dir=North) kgraph knode = 
	let port = new kport kgraph in
	port#setWidth 5.0;
	port#setHeight 5.0;
	begin match dir with
	| North -> 
		port#addProperty (PersistentEntry.createPortNorth ());
	| South -> 
		port#addProperty (PersistentEntry.createPortSouth ());
	| East ->
		port#addProperty (PersistentEntry.createPortEast ());
	| West ->
		port#addProperty (PersistentEntry.createPortWest ());
	end;
	port#setNode knode;
	addPortOffset port ofs;
	let cont = new containerRendering in
	cont#setContainer Ellipse;
	cont#addStyle (create_style (LineWidth 1.3));
	cont#addStyle (create_style (Foreground (create_coloring (opLineColor custom))));
	cont#addStyle (create_style (Background (create_coloring (opLineColor custom))));
	cont#addStyle (create_style ~on_sel:true (LineWidth 1.5));
	port#addData cont;
	port



let simpleInitNode ?(custom=default_informations) kgraph name = 
	let node = defaultNode ~order:true kgraph in
	let cont = simpleOpContWtT ~custom:custom () in
	if name <> "" then begin
		let t = simpleText ~custom:custom "init" 0.5 0.5 in
		t#setPlacement (place ~top:0.0 ~left:5.0 ~right:5.0 ~bottom:12.0 ());	
		cont#addContainerRendering t;
		let t = simpleText ~custom:custom name 0.5 0.5 in
		t#setPlacement (place ~top:12.0 ~left:5.0 ~right:5.0 ~bottom:0.0 ());
		cont#addContainerRendering t;
	end else begin
		let t = simpleText ~custom "init" 0.5 0.5 in
		t#setPlacement (place ~top:0.0 ~left:5.0 ~right:5.0 ~bottom:0.0 ());
		cont#addContainerRendering t;
	end;
	node#addData cont;
	resetPortsSurrounding node;
	node


let simpleDiscNode ?(custom=default_informations) kgraph = 
	simpleOpNode ~custom:custom ~ho_mar:5.0 kgraph "disc" 0.5 0.5


let simpleTestNode ?(custom=default_informations) kgraph = 
	simpleOpNode ~custom:custom ~ho_mar:5.0 kgraph "?" 0.5 0.5

let simpleInvStateNode kgraph = 
	let node = new knode kgraph in
	let cont = new containerRendering in
	node#setWidth 1.0;
	node#setHeight 1.0;
	cont#addStyle (create_style (Invisibility));
	node#addProperty (PersistentEntry.layerConstraint "FIRST_SEPARATE");
	node#addData cont;
	node

let simpleAppNode ?(custom=default_informations) kgraph = 
	simpleOpNode ~custom:custom ~ho_mar:5.0 kgraph "App" 0.5 0.5

let arrowBox ?(custom=default_informations) ?(dim=5.0) cont rightDelta = 
	let t = simpleText ~custom:custom ~s:8 "-" 0.5 0.5 in
	let pd = new PointPlacementData.pointPlacementData in
	pd#setMinHeight dim;
	pd#setMinWidth (0.5 *. dim);
	pd#setHorizontalAlignment CENTER;
	pd#setVerticalAlignment CENTER;
	let c1 = Point.create_coord ~pos_val:(Abs (dim *. (rightDelta +. 0.5))) Right in
	let c2 = Point.create_coord ~pos_val:(Rel 0.5) Top in
	let point = Point.create_point c1 c2 in
	pd#setRefPoint point;
	t#setPlacement (Point pd);
	cont#addContainerRendering t;
	
	let t = simpleText ~custom:custom ~s:8 "&gt;" 0.5 0.5 in
	let pd = new PointPlacementData.pointPlacementData in
	pd#setMinHeight dim;
	pd#setMinWidth (1. *. dim);
	pd#setHorizontalAlignment CENTER;
	pd#setVerticalAlignment CENTER;
	let c1 = Point.create_coord ~pos_val:(Abs (dim *. (rightDelta))) Right in
	let c2 = Point.create_coord ~pos_val:(Rel 0.52) Top in
	let point = Point.create_point c1 c2 in
	pd#setRefPoint point;
	t#setPlacement (Point pd);
	cont#addContainerRendering t


let partialAppNode ?(custom=default_informations) kgraph name num num_taken = 
	let node = defaultNode ~order:true kgraph in
	let cont = simpleOpContWtT ~custom:custom () in
	let rec addBox n = match n with
	| -1 -> ()
	| _ -> 
		constrBox ~custom:custom ~no_line:(n < num - num_taken) cont (2.5 +. 4.5 *. (float_of_int n)) true;
		arrowBox ~custom:custom cont (1.5 +. 4.5 *. (float_of_int n));
		addBox (n-1)
	in
	addBox (num - 1);
	
	let num = (float_of_int num) in
	let t = simpleText ~s:8 ~custom:custom name 0.5 0.5 in
	t#setPlacement (place ~top:0.0 ~left:5.0 ~right:((2. +. 4.5 *. num) *. 5.0) ~bottom:0.0 ());
	cont#addContainerRendering t;
	node#addData cont;
	
	let right_ofs = 19.85 +. 22.5 *. (num -. (float_of_int num_taken)) in

	node#addProperty (PersistentEntry.create_property "org.eclipse.elk.spacing.individual"
	("org.eclipse.elk.spacing.portPort:22.5;,;org.eclipse.elk.spacing.portsSurrounding:[top=0.0,left=0.0,bottom=0.0,right="^(string_of_float right_ofs)^"]"));
	node#addProperty (PersistentEntry.create_property "org.eclipse.elk.portAlignment.north" "END");

	node

(* end for z *)

let function_node ?(order=false) ?(custom=default_informations) ?(res=false) ?(aut=false) ?(m=false) kgraph name layer = 
	let main_node= defaultNode ~order:order kgraph in
	if aut then begin
		main_node#addProperty (PersistentEntry.create_property "org.eclipse.elk.edgeRouting" "SPLINES"); 
		(*main_node#addProperty (PersistentEntry.create_property "org.eclipse.elk.hierarchyHandling" "INCLUDE_CHILDREN");
	*)(*end else begin
		main_node#addProperty (PersistentEntry.create_property "org.eclipse.elk.hierarchyHandling" "SEPARATE_CHILDREN");
*)	end;	
	let cont = new containerRendering in
	let ca = new data in
	ca#setPlacement (place ());
	cont#setChildArea ca;
	cont#addAction (Actions.create_actionCollapse ());
	let background = 
		if not m then 
			if aut then aut_color layer 
			else if res then
				reset_color layer
			else fct_color layer
		else match_color layer 
	in

	addPortSpace main_node;
	
	cont#addStyle (create_style (Background (create_coloring background)));
	cont#addStyle (create_style (Foreground (create_coloring (opLineColor custom))));
	if (m || res) then cont#addStyle (create_style (LineStyle DASH));
	
	if m then main_node#addProperty (PersistentEntry.create_property "org.eclipse.elk.layered.considerModelOrder.strategy" "PREFER_NODES");
	cont#addContainerRendering (functionTitle ~custom:custom name);
	cont#addStyle (create_style (Shadow (4.0 , 4.0)));
	main_node#addData cont;
	
	main_node#addProperty (PersistentEntry.expand (string_of_bool (!InterLib_options.do_show_all || (layer=0))));
	main_node#addProperty (PersistentEntry.nodeSize "[NODE_LABELS, PORTS, PORT_LABELS, MINIMUM_SIZE]");
	main_node#addProperty (PersistentEntry.activatePartition ());
	if layer > 0 then main_node#addProperty (PersistentEntry.portLabelPlacement "[INSIDE, NEXT_TO_PORT_IF_POSSIBLE]");
	main_node

let state_color layer =
	layered_color 242 224 224 layer

let stateNode ?(custom=default_informations) ?(first=false) ?(init=false) kgraph name layer = 
	let node = defaultStateNode ~layer:(if first then "FIRST" else "NONE") kgraph in
	node#addProperty (PersistentEntry.activatePartition ());
	let cont = new containerRendering in
	let ca = new data in
	ca#setPlacement (place ());
	cont#setChildArea ca;
	cont#addAction (Actions.create_actionCollapse ());

	addPortSpace node;
	

	let color = state_color layer in

	cont#addStyle (create_style (Background (create_coloring color)));
	cont#addStyle (create_style (Foreground (create_coloring (opLineColor custom))));
	cont#addStyle (create_style (LineWidth (if init then 3.0 else 1.0)));
	cont#addStyle (create_style ~on_sel:true (LineWidth (if init then 4.5 else 1.5)));
	cont#addContainerRendering (functionTitle ~custom:custom ~ho_mar:6.0 name);
		
	node#addProperty (PersistentEntry.expand (string_of_bool (!InterLib_options.do_show_all || (layer=0))));
	node#addProperty (PersistentEntry.nodeSize "[NODE_LABELS, PORTS, PORT_LABELS, MINIMUM_SIZE]");

	cont#setContainer (RoundRect (10.0 , 10.0));
	cont#addStyle (create_style (Shadow (4.0,4.0)));
	node#addData cont;
	node


let sync_color layer = 
	layered_color 210 210 200 layer

let terminalSyncNode kgraph = 
	let node = defaultStateNode ~layer:"LAST" kgraph in
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

let simpleSyncNode ?(custom=default_informations) ?(init=false) kgraph =
	let node = defaultStateNode ~layer:(if init then "FIRST" else "NONE") kgraph in
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
	cont#addStyle (create_style (Foreground (create_coloring (opLineColor custom))));
	cont#addStyle (create_style (LineWidth (if init then 3.0 else 1.0)));
	cont#addStyle (create_style ~on_sel:true (LineWidth (if init then 4.5 else 1.5)));
	cont#addStyle (create_style (Shadow (4.0,4.0)));	
	node#addProperty (PersistentEntry.expand (string_of_bool (!InterLib_options.do_show_all)));
	node#addProperty (PersistentEntry.nodeSize "[NODE_LABELS, PORTS, PORT_LABELS, MINIMUM_SIZE]");
	resetPortsSurrounding node;
	cont#setContainer (RoundRect (10.0 , 10.0));
	node#addData cont;
	node

let simpleSeqBlockNode ?(custom=default_informations) kgraph name = 
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
	cont#addStyle (create_style (Foreground (create_coloring (opLineColor custom))));
	cont#addStyle (create_style (LineStyle DASH));
	cont#addContainerRendering (functionTitle ~custom:custom name);
	cont#addStyle (create_style (Shadow (4.0 , 4.0)));
	node#addData cont;
	
	node#addProperty (PersistentEntry.expand (string_of_bool (!InterLib_options.do_show_all)));
	
	node


let simpleBubleNode ?(init=false) kgraph = 
	let node = defaultStateNode ~layer:(if init then "FIRST" else "NONE") kgraph in
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
	let t = simpleText ~s:8 "Pause" 0.5 0.5 in
	let white = create_color 255 255 255 in
	t#addStyle (create_style (Foreground (create_coloring white)));
	cont#addContainerRendering t;
	node#addData cont;
	node


let ramNode ?(custom=default_informations) kgraph = 
	let node = simpleOpNode ~custom:custom ~title:true ~order:true kgraph "ram" 0.5 0.1 in
	node#setHeight 130.0;
	node#setWidth 70.0;
	node#addProperty (PersistentEntry.portLabelPlacement "[INSIDE,NEXT_TO_PORT_IF_POSSIBLE]");
	node

let romNode ?(custom=default_informations) kgraph = 
	let node = simpleOpNode ~custom:custom ~center:true ~title:true ~order:true kgraph "rom" 0.5 0.5 in
	node#setHeight 60.0;
	node#setWidth 30.0;
	node


type thickness_type = 
	| Gu_Simple
	| Gu_Mult
	| Gu_Big

let junctionThickness tt = 
	match tt with
	| Gu_Simple -> 4.0
	| Gu_Mult -> 5.0
	| Gu_Big -> 7.0

let junction ?(custom=default_informations) thick =
	let cont = new containerRendering in
	cont#setContainer Ellipse;
	let color = opLineColor custom in
	cont#addStyle (create_style (Background (create_coloring color)));
	cont#addStyle (create_style (Foreground (create_coloring color)));
	let pd = new PointPlacementData.pointPlacementData in
	pd#setVerticalAlignment CENTER;
	pd#setHorizontalAlignment CENTER;
	let size = junctionThickness thick in
	pd#setMinWidth size;
	pd#setMinHeight size;
	cont#setPlacement (Point pd);
	cont


let arrow_decorator ?(custom=default_informations) ?(tiny=false) ?(h=false) alone = 
	let place = new DecoratorPlacementData.decoratorPlacementData in
	if alone then 
		place#setAbsolute (-4.0)
	else if h then place#setAbsolute (-14.0) else place#setAbsolute (-12.0);
	place#setXOffset (if tiny then (-2.0) else (-8.0));
	place#setYOffset (if tiny then (-2.0) else (-4.0));
	place#setRotateWithLine true;
	place#setWidth (if tiny then 6.0 else 12.0);
	place#setHeight (if tiny then 4.0 else 8.0);
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
	cont#addStyle (create_style (LineWidth (if tiny then 1.0 else 1.85)));
	cont#addStyle (create_style ~on_sel:true (LineWidth (if tiny then 1.0 else 3.0)));
	let normal_color = 
		if custom#getInCycle || (not tiny) then opLineColor custom
		else create_color 100 255 100 
	in
	cont#addStyle (create_style (Background (create_coloring normal_color )));
	cont#addStyle (create_style (Foreground (create_coloring normal_color )));
	let select_color = 
		if custom#getInCycle then opLineColor custom
		else if tiny then create_color 100 255 100
		else create_color 100 100 255 in
	cont#addStyle (create_style ~on_sel:true (Background (create_coloring select_color)));
	cont#addStyle (create_style ~on_sel:true (Foreground (create_coloring select_color)));
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
	place#setAbsolute (-5.);
	place#setXOffset (-6.);
	place#setYOffset (-6.);
	place#setWidth 12.0;
	place#setHeight 12.0;
	place#setRelative 1.0;

	let h = simpleText ~s:7 "H" 0.5 0.5 in

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

let labelOfEdgeLabel ?(custom=default_informations) kgraph name pos = 
	let label = new Klabel.klabel kgraph in
	label#setText name;
	let cont = new containerRendering in
	cont#setContainer (Text name); 
	cont#addStyle (create_style (Foreground (create_coloring (opLineColor custom))));
	label#addData cont;
	begin match pos with
	| Intermediate_graph.Tail ->
		label#addProperty (PersistentEntry.edgeLabelPlacement "TAIL");
	| Center ->
		label#addProperty (PersistentEntry.edgeLabelPlacement "CENTER");
	| Head ->
		label#addProperty (PersistentEntry.edgeLabelPlacement "HEAD");
	| Undef ->
		()
	end;
	label

let inlinedLabel ?(custom=default_informations) kgraph name =
	let label = new Klabel.klabel kgraph in
	label#setText name;
	let cont = new containerRendering in
	let color = create_color 240 240 240 in
	cont#setContainer (RoundRect (8.0,8.0));
	cont#addStyle (create_style (LineStyle DASH));
	cont#addStyle (create_style (Background (create_coloring color)));
	cont#addContainerRendering (simpleText ~ho_mar:5.0 name 0.5 0.5);
	label#addData cont;
	label#addProperty (PersistentEntry.create_property "org.eclipse.elk.edgeLabels.inline" "true");
	label

let portLabel ?(custom=default_informations) kgraph name = 
	let label = new Klabel.klabel kgraph in
	label#setText name;
	let cont = new containerRendering in
	cont#setContainer (Text name);
	cont#addStyle (create_style (Foreground (create_coloring (opLineColor custom))));
	label#addData cont;
	label

let seq_edge ?(half=false) ?(sourcePort=None) ?(targetPort=None) kgraph source target = 
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
	edge

let automaton_edge ?(custom=default_informations) ?(sourcePort=None) ?(targetPort=None) kgraph e_type source target = 
	let edge = new kedge kgraph in
	edge#setSource source;
	edge#setTarget target;
	begin match sourcePort with
	| Some p -> edge#setSourcePort p
	| _ -> ()
	end;
	begin match targetPort with
	| Some p -> edge#setTargetPort p
	| _ -> ()
	end;
	
	let cont = new containerRendering in
	cont#setContainer Spline;
	cont#addStyle (create_style (LineWidth 2.));
	cont#addStyle (create_style ~on_sel:true (LineWidth 3.));
	let color = create_color 100 100 255 in
	cont#addStyle (create_style (Foreground (create_coloring (opLineColor custom))));
	cont#addStyle (create_style ~on_sel:true (Foreground (create_coloring color)));
	begin match e_type with
	| Intermediate_graph.Aut_begin ->
		cont#addContainerRendering (red_dot true);
		cont#addContainerRendering (arrow_decorator ~custom:custom true);
	| Aut_end ->
		cont#addContainerRendering (red_dot false);
		cont#addContainerRendering (arrow_decorator ~custom:custom false);
	| Aut_begin_history ->
		cont#addContainerRendering (red_dot true);
		cont#addContainerRendering (history_dot ());
		cont#addContainerRendering (arrow_decorator ~custom:custom ~h:true false);
	| Aut_end_history ->
		cont#addContainerRendering (history_dot ());
		cont#addContainerRendering (arrow_decorator ~custom:custom ~h:true false);
	| Aut_first_half ->
		()
	| Aut_first_half_begin ->
		cont#addContainerRendering (red_dot true);
	| Aut_second_half_begin ->
		cont#addContainerRendering (arrow_decorator ~custom:custom true);
	| Aut_second_half_end ->
		cont#addContainerRendering (red_dot false);
		cont#addContainerRendering (arrow_decorator ~custom:custom false);
	| Aut_second_half_history ->
		cont#addContainerRendering (history_dot ());
		cont#addContainerRendering (arrow_decorator ~custom:custom ~h:true false);
	| Aut_port -> 
		cont#addContainerRendering (arrow_decorator ~custom:custom true);
	| _ -> assert false
	end;
	edge#addData cont;
	edge

let lineThickness tt = 
	match tt with
	| Gu_Simple -> 1.0
	| Gu_Mult -> 1.7
	| Gu_Big -> 2.5

let lineSelThickness tt = 
	match tt with
	| Gu_Simple -> 1.5
	| Gu_Mult -> 2.5
	| Gu_Big -> 4.0

let new_edge ?(custom=default_informations) ?(thick=Gu_Simple) kgraph sourceNode sourcePort targetNode targetPort = 
	let edge = new kedge kgraph in
	edge#setSource sourceNode;
	edge#setSourcePort sourcePort;
	edge#setTarget targetNode;
	edge#setTargetPort targetPort;
	let cont = new containerRendering in
	cont#setContainer (PolyLine []);
	
	cont#addStyle (create_style (LineWidth (lineThickness thick)));
	cont#addStyle (create_style ~on_sel:true (LineWidth (lineSelThickness thick)));
	let color = create_color 100 100 255 in
	cont#addStyle (create_style (Foreground (create_coloring (opLineColor custom))));
	cont#addStyle (create_style ~on_sel:true (Foreground (create_coloring color)));
	cont#addJunction (junction ~custom:custom thick);
	edge#addData cont;
	edge

let linkEdge ?(custom=default_informations) kgraph sourceNode sourcePort targetNode targetPort =
	let edge = new kedge kgraph in
	edge#setSource sourceNode;
	edge#setSourcePort sourcePort;
	edge#setTarget targetNode;
	edge#setTargetPort targetPort;
	let cont = new containerRendering in
	cont#setContainer (PolyLine []);
	cont#addStyle (create_style (LineWidth 1.0));
	let color = create_color 100 255 100 in
	cont#addStyle (create_style (Foreground (create_coloring (if custom#getInCycle then opLineColor custom else color))));
	edge#addProperty (PersistentEntry.create_property "org.eclipse.elk.noLayout" "true");
	cont#addContainerRendering (arrow_decorator ~custom:custom ~tiny:true true);
	edge#addData cont

let init_kgraph () = 
	let kgraph = new kgraph in
	
	kgraph#addProperty (PersistentEntry.addPortSpace "10.0" "0.0" "10.0" "10.0");
	kgraph

let main _ = 
	let kgraph = new kgraph in
(*	
	let _ = simpleOrNode kgraph in
	let _ = simpleAndNode kgraph in
	let _ = simpleXorNode kgraph in
	
	let _ = simpleNandNode kgraph in

	let _ = simpleRegNode kgraph in
	
	let _ = simpleBufferNode kgraph in

	let _ = simpleMuxNode kgraph in

	let _ = simpleConstNode kgraph "hiaaaaaao" in

	let _ = simpleSelectNode kgraph "2" in
	let _ = simpleSliceNode kgraph "0" "n" in
	let _ = simpleConcatNode kgraph in

	let _ = function_node kgraph "fulladder" 0 in
	let _ = function_node kgraph "fulladder"  1 in
	let _ = function_node kgraph "fulladder"  2 in
	let _ = function_node kgraph "fulladder" 3 in
	let _ = function_node kgraph "fulladder" 4 in
	let _ = function_node kgraph "fulladder" 5 in
	let _ = function_node kgraph "fulladder" 15 in

	let _ = ramNode kgraph in

	let main = function_node kgraph "main" 0 in

	let cond = simpleCondNode kgraph in
	
	cond#setParent main;

	let matchNode = simpleMatchNode kgraph in
	matchNode#setParent main;
	
	let _ = simpleBubleNode kgraph in 
	let _ = simpleFbyNode kgraph in

	let _ = simpleTupleNode kgraph in

	let _ = simpleSinkNode kgraph "ala" in

	let _ = stateNode kgraph "oo" 0 in
	
	let _ = terminalSyncNode kgraph in
*)
(*	let node0 = function_node kgraph "main" 0 in	
	let node1 = function_node ~aut:true kgraph "aut" 0 in
	let node2 = stateNode kgraph "A" 2 in
	let node3 = simpleOrNode kgraph in
	node2#setParent node1;
	node3#setParent node2;
	node1#setParent node0;
	let p1 = visibleInputPort kgraph node1 in 
	let p3 = visibleInputPort kgraph node3 in
	linkEdge kgraph node1 p1 node3 p3;	
*)
	let _ = simpleDerNode kgraph "no" in
	let _ = simpleDerNode kgraph "qroignqerionqergq" in
	let oc = open_out "basic.kgx" in

	let ff = Format.formatter_of_out_channel oc in
	Kgraph_printer.graph_to_kgx ff kgraph

