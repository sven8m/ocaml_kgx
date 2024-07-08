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

(** Graph_utils describes a variety of different knodes and containers needed for the creation of the needed kgraph.

In general the option [custom] is to give an [iInformation] which will be used to define the color of the lines. *)

let default_informations = new iInformation

(** [layered_color red green blue layer] returns the corresponding color, being more dark in more deep layers*)
let layered_color red green blue layer = 
	let layer = if layer >= 5 then 5 else layer in
	create_color (max 0 (red - 10 * layer)) (max 0 (green - 10 * layer)) (max 0 (blue - 10 * layer))

(** [aut_color layer] creates the color for an automaton.*)
let aut_color _ = 
	layered_color 240 240 255 0

(** [fct_color layer] creates the color for a function node. *)
let fct_color layer = 
	layered_color 224 224 242 layer

(** [match_color layer] creates the color for a match node. *)
let match_color layer = 
	layered_color 200 200 242 layer

(** [reset_color layer] creates the color for a reset node. *)
let reset_color layer = 
	layered_color 190 200 250 layer

(** [operation_color ()] creates the color for an operation node. *)
let operation_color () = 
	layered_color 200 200 250 0

(** [cond_color ()] creates the color for a conditional node. *)
let cond_color () = 
	create_color 200 250 225 

(** [place ()] creates an area placementa data with absolute offset as options.

Default is [top] at [15.0], [left] at [10.0], [right] at [10.0] and [bottom] at [10.0]. *)
let place ?(top=15.0) ?(left=10.0) ?(right=10.0) ?(bottom=10.0) () =
	let p = new AreaPlacementData.areaPlacementData in
	let c1 = create_coord ~pos_val:(Abs left) Left in
	let c2 = create_coord ~pos_val:(Abs top) Top in
	p#setTopLeft (create_point c1 c2);
	let c3 = create_coord ~pos_val:(Abs right) Right in
	let c4 = create_coord ~pos_val:(Abs bottom) Bottom in
	p#setBotRight (create_point c3 c4);
	Area p

(** [absolutePointPlacement x y] creates a point placement data with point placement being absolute in [x] from Left and [y] from Top. 
Option is to give a minimum width and height. *)
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

(** [centerPlacement x y] creates a point placement data with placement being relative in [x] from Left and [y] from Top.
Option is to give a horizontal and vertical margin. *)
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

(** [opLineColor custom] calculates the color a line should have, depending if the line is on a cycle or in dead code. *)
let opLineColor custom = 
	if custom#getInCycle then create_color 255 20 20 
	else if custom#getDead then
		create_color 100 100 100
	else create_color 0 0 0

(** Creates a very basic knode with width and height 20.
	
	Option [order] if the port order should be considered (default [false]).
	
	Option [layer] with value "LAST", "FIRST" or "NONE" if the node should be placed left or right of the layout. 
		
	Option [side] if the port side should be considered (default [true]). ([order] has more priority than [side])*)
let defaultNode ?(layer) ?(order=false) ?(side=true) (kgraph : kgraph) = 
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

(** Creates a basic node, as for defaultNode, but with no port side constraint. *)
let defaultStateNode ?(layer) (kgraph : kgraph) = 
	let node = new knode kgraph in
	node#setWidth 20.0;
	node#setHeight 20.0;
	node#addProperty (PersistentEntry.separateComponent false);
	begin match layer with
	| Some s -> node#addProperty (PersistentEntry.layerConstraint s)
	| None -> ()
	end;
	node

(** Creates a rectangular container. 

Option [inv] to make it invisible (default [false]).

Option [spe_back] to give a special background color. *)
let opContWtT ?(spe_back=None) ?(inv=false) ?(custom=default_informations) () =
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

(** Creates a basic node for operations consisting of a [defaultNode] with container [opContWtT] *)
let opNodeWtT ?(custom=default_informations) kgraph =
	let node = defaultNode kgraph in
	node#addData (opContWtT ~custom:custom ());
	node

(** [functionTitle s] Creates a text rendering with text s that is a function title, which will have constant size on the screen even when we are zoomed out.
	The text is centered from the left and 15 from the top.

	Option [center] if the text should be centered from the top.

	Option [ho_mar] to put an horizontal margin. *)
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

(** [textContainer text x y] creates a text rendering with text [text] and a centered placement from ([x],[y]).

	Option [s] to change the font size (default [11])

	Options [ho_mar] and [ver_mar] to put a vertical and horizontal margin (default [0]) *)
let textContainer ?(custom=default_informations) ?(s=11) ?(ho_mar=0.0) ?(ver_mar) text relX relY =
	let c = new containerRendering in
	c#setContainer (Text text);
	c#addStyle (create_style ~on_sel:true Bold);
	c#addStyle (create_style (FontSize s));
	c#addStyle (create_style (Foreground (create_coloring (opLineColor custom))));
	c#setPlacement (centerPlacement ~ho_mar:(Some ho_mar) ~ver_mar:ver_mar relX relY);
	c

(** [addPortOffset port ofs] adds a border offset property.*)
let addPortOffset kport ofs = 
	match ofs with
	| None -> ()
	| Some ofs ->
		kport#addProperty (PersistentEntry.borderOffset ofs)


(** [defaultPortNode kgraph knode] creates a port for the given knode, with default height and width 5.0 (can be changed as option),
and no given container (i.e default container from KLighD is used).

Also as option, the possibility to add an offset for the port. *)
let defaultPortNode ?(ofs=None) ?(height=5.0) ?(width=5.0) (kgraph : kgraph) knode = 
	let port = new kport kgraph in
	port#setNode knode;
	port#setHeight height;
	port#setWidth width;
	addPortOffset port ofs;
	port

(** [defaultPortContainer ()] creates a plain rectangular container with color depending on the [custom] option. *)
let defaultPortContainer ?(custom=default_informations) () = 
	let cont = new containerRendering in
	let color = opLineColor custom in
	cont#addStyle (create_style (Background (create_coloring color)));
	cont#addStyle (create_style (Foreground (create_coloring color)));
	cont

(** [invisiblePort kgraph knode] creates a port for [knode] with width and height 0.01, with default container. *)
let invisiblePort ?(custom=default_informations) ?(ofs=None) kgraph knode =
	let port = defaultPortNode ~ofs:ofs ~height:0.01 ~width:0.01 kgraph knode in
	let cont = defaultPortContainer ~custom:custom () in
	port#addData cont;
	begin match ofs with
	| None -> addPortOffset port (Some (-0.01));
	| Some _ -> ()
	end;
	port

(** [visiblePort kgraph knode] creates a port for [knode] with width and height 5.0, with default container. *)
let visiblePort ?(custom=default_informations) ?(ofs=None) kgraph knode = 
	let port = defaultPortNode ~ofs:ofs ~height:5.0 ~width:5.0 kgraph knode in
	let cont = defaultPortContainer ~custom:custom () in
	port#addData cont;
	port

(** [notPortContainer ()] creates a an ellipse container empty in the middle, with front color depending on [custom]. *)
let notPortContainer ?(custom=default_informations) () = 
	let cont = new containerRendering in
	cont#setContainer Ellipse;
	cont#addStyle (create_style (LineWidth 1.3));
	cont#addStyle (create_style (Foreground (create_coloring (opLineColor custom))));
	cont#addStyle (create_style ~on_sel:true (LineWidth 1.5));
	cont

(** [notPort kgraph knode] creates a port for [knode] with width and height 5.0, with container [notPortContainer]. *)
let notPort ?(custom=default_informations) ?(ofs=None) kgraph knode = 
	let port = defaultPortNode ~ofs:ofs ~height:5.0 ~width:5.0 kgraph knode in
	let cont = notPortContainer ~custom:custom () in
	port#addData cont;
	port

(** [notPortContainer ()] creates a container rendering consisting of a rectangular operation node with text [?], with color depending on [custom]. *)
let questionPortContainer ?(custom=default_informations) () = 
	let cont = new containerRendering in
	cont#setContainer Rect;
	cont#addStyle (create_style (LineWidth 1.0));
	cont#addStyle (create_style (Foreground (create_coloring (opLineColor custom))));
	cont#addStyle (create_style ~on_sel:true (LineWidth 1.5));
	let back_color = create_color 200 200 250 in
	cont#addStyle (create_style (Background (create_coloring back_color)));
	cont#addContainerRendering (textContainer ~custom:custom ~s:6 "?" 0.5 0.5);
	cont

(** [questionPort kgraph knode] creates a port for [knode] with width and height 10.0, with container [questionPortContainer]. *)
let questionPort ?(custom=default_informations) ?(ofs=None) kgraph knode = 
	let port = defaultPortNode ~ofs:ofs ~height:10.0 ~width:10.0 kgraph knode in
	let cont = questionPortContainer ~custom:custom () in
	port#addData cont;
	port

(** [notPortContainer ()] creates an ellipse container with filled background, with color depending on [custom]. *)
let bublePortContainer ?(custom=default_informations) () = 
	let cont = new containerRendering in
	cont#setContainer Ellipse;
	cont#addStyle (create_style (LineWidth 1.3));
	cont#addStyle (create_style (Foreground (create_coloring (opLineColor custom))));
	cont#addStyle (create_style (Background (create_coloring (opLineColor custom))));
	cont#addStyle (create_style ~on_sel:true (LineWidth 1.5));
	cont
	
(** [bublePort kgraph knode] creates a port for [knode] with width and height 5.0, with container [bublePortContainer]. *)
let bublePort ?(custom=default_informations) ?(ofs=None) kgraph knode = 
	let port = defaultPortNode ~ofs:ofs ~height:5.0 ~width:5.0 kgraph knode in
	let cont = bublePortContainer ~custom:custom () in
	port#addData cont;
	port

(** [halfCirclePortContainer ()] creates a container being a half circle, with color depending on [custom]. *)
let halfCirclePortContainer ?(custom=default_informations) () = 
	let cont = new containerRendering in
	cont#setContainer (Arc (0.0 , 180.0));
	cont#addStyle (create_style (Foreground (create_coloring (opLineColor custom))));
	cont#addStyle (create_style (Background (create_coloring (opLineColor custom))));	
	cont

(** [halfCirclePort kgraph knode] creates a port for [knode] with width and height 7.5, with container [halfCirclePortContainer]. *)
let halfCirclePort ?(custom=default_informations) ?(ofs=None) kgraph knode =
	let ofs = match ofs with
	| None -> Some (-.3.75)
	| Some f -> Some (f -. 3.75)
	in
	let port = defaultPortNode ~ofs:ofs ~height:7.5 ~width:7.5 kgraph knode in
	let cont = halfCirclePortContainer ~custom:custom () in
	port#addData cont;
	port

(** [loopPortContainer ()] creates a container a loop with arrow, with color depending on [custom]. *)
let loopPortContainer ?(custom=default_informations) () = 
	let cont = new containerRendering in
	cont#setContainer (Arc (-45.0, 255.0));
	cont#addStyle (create_style (LineWidth 0.9));


	let c1 = Point.create_coord ~pos_val:(Rel 0.4) Left in
	let c2 = Point.create_coord Top in
	let p1 = Point.create_point c1 c2 in

	let c3 = Point.create_coord ~pos_val:(Rel 0.4) Top in
	let p2 = Point.create_point c1 c3 in

	let c4 = Point.create_coord Left in
	let p3 = Point.create_point c4 c3 in

	let c5 = Point.create_coord Right in
	let c6 = Point.create_coord Bottom in
	let p4 = Point.create_point c5 c6 in

	let arrow = new containerRendering in
	arrow#setContainer (Polygon [p1;p2;p3;p4]);
	
	let pd = new PointPlacementData.pointPlacementData in
	pd#setMinHeight 5.;
	pd#setMinWidth 6.;
	pd#setHorizontalAlignment CENTER;
	pd#setVerticalAlignment CENTER;
	let c1 = Point.create_coord ~pos_val:(Rel 0.20) Left in
	let c2 = Point.create_coord ~pos_val:(Rel 0.75) Top in
	let point = Point.create_point c1 c2 in
	pd#setRefPoint point;
	arrow#setPlacement (Point pd);
	arrow#addStyle (create_style JoinRound);
	arrow#addStyle (create_style (LineWidth 0.4));
	arrow#addStyle (create_style (Foreground (create_coloring (opLineColor custom))));
	arrow#addStyle (create_style (Background (create_coloring (opLineColor custom))));
	cont#addContainerRendering arrow;
	cont

(** [loopPort kgraph knode] creates a port for [knode] with width and height 10.0, with container [loopPortContainer]. *)
let loopPort ?(custom=default_informations) ?(ofs=None) kgraph knode =
	let ofs = match ofs with
	| None -> Some (-6.)
	| Some o -> Some (o -. 6.)
	in
	let port = defaultPortNode ~ofs:ofs ~height:10.0 ~width:10.0 kgraph knode in
	let cont = loopPortContainer ~custom:custom () in
	port#addData cont;
	port

(** [createPort kgraph knode] creates a [port] for the corresponding [knode].

As option the [offset] (default [None]), the [look] which will choose the corresponding rendering (default [Invisible]), 
and the [side] constraint (default [Undefined] i.e no constraint)*)
let createPort ?(custom=default_informations) ?(ofs=None) ?(look=Invisible) ?(side=Undefined) kgraph knode = 
	let port = match look with
	| Invisible -> invisiblePort ~custom:custom kgraph ~ofs:ofs knode 
	| Visible -> visiblePort ~custom:custom kgraph ~ofs:ofs knode
	| Not -> notPort ~custom:custom ~ofs:ofs kgraph knode 
	| Question -> questionPort ~custom:custom ~ofs:ofs kgraph knode
	| Buble -> bublePort ~custom:custom ~ofs:ofs kgraph knode
	| HalfCircle -> halfCirclePort ~custom:custom ~ofs:ofs kgraph knode
	| Loop -> loopPort ~custom:custom ~ofs:ofs kgraph knode
	in
	begin match side with
	| East | Output ->
		port#addProperty (PersistentEntry.createPortEast ())
	| West | Input-> 	port#addProperty (PersistentEntry.createPortWest ())
	| North | Control ->	port#addProperty (PersistentEntry.createPortNorth ())
	| South ->	port#addProperty (PersistentEntry.createPortSouth ())	
	| Undefined -> ()
	end;
	port

(** [resetPortsSurrounding node] resets the additional port space for this node.*)
let resetPortsSurrounding (node : Knode.knode) = 
	node#addProperty (PersistentEntry.create_property "org.eclipse.elk.spacing.individual" "org.eclipse.elk.spacing.portsSurrounding:[top=0.0,left=0.0,bottom=0.0,right=0.0]")

(** [addPortSpace node] add a port space for the children , 12 from top, 0 from bottom and 10 from left and right. *)
let addPortSpace (node : Knode.knode) = 
	node#addProperty (PersistentEntry.addPortSpace "12.0" "0.0" "10.0" "10.0")

(** [opNode text x y] creates a node for arithmetic or basic operations, with text [text] positioned centered at relative [x] and [y].

	Option [center] to use with option [title] to center the function title.

	Option [title] to specify if the text should be a function title. (i.e stay at constant size even when woomed out).

	Option [ho_mar] to specify if the text should have an horizontal margin.
	
	Option [order] to specify that the ports have fixed order.
	
	Option [inv] to make the node invisible (default [false]).*)
let opNode ?(inv=false) ?(custom=default_informations) ?(center=false) ?(title=false) ?(ho_mar=0.0) ?(order=false) (kgraph : Kgraph.kgraph) text relX relY= 
	let node = defaultNode ~order:order kgraph in
	let cont = opContWtT ~inv:inv ~custom:custom () in
	if not inv then cont#addContainerRendering (if title then functionTitle ~custom:custom ~center:center text else textContainer ~custom:custom ~ho_mar:ho_mar text relX relY);
	node#addData cont;
	resetPortsSurrounding node;
	node


let linkNode ?(custom=default_informations) kgraph = 
	let node = opNode ~inv:(not !InterLib_options.do_show_all) ~custom:custom kgraph "L" 0.5 0.5 in
	node#addProperty (PersistentEntry.create_property "org.eclipse.elk.noLayout" (string_of_bool (not !InterLib_options.do_show_all)));
	node

(** [xorNode kgraph] creates a rectangle node with text [=1]*)
let xorNode ?(custom=default_informations) kgraph = 
	opNode ~custom:custom kgraph "=1" 0.5 0.6

(** [andNode kgraph] creates a rectangle node with text [&]*)
let andNode ?(custom=default_informations) kgraph = 
	opNode ~custom:custom kgraph "&amp;" 0.5 0.6

(** [orNode kgraph] creates a rectangle node with text [!=1]*)
let orNode ?(custom=default_informations) kgraph = 
	opNode ~custom:custom kgraph "&#x2265;1" 0.5 0.6

(** [selectNode kgraph i] creates a rectangle node with text \[i\]. *)
let selectNode ?(custom=default_informations) kgraph i = 
	opNode ~custom:custom ~ho_mar:2.0 kgraph ("["^i^"]") 0.5 0.5

(** [sliceNode kgraph i j] creates a rectangle node with text \[i.j\]. *)
let sliceNode ?(custom=default_informations) kgraph i j = 
	opNode ~custom:custom ~ho_mar:2.0 kgraph ("["^i^"."^j^"]") 0.5 0.5

(** [concatNode kgraph] creates a rectangle node with text . *)
let concatNode ?(custom=default_informations) kgraph =
	opNode ~custom:custom ~order:true kgraph "." 0.5 0.5

(** [rectangleAsPolygon ()] creates a container that has the form of a rectangle but is formed by a polygon. *)
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

(** [nandNode kgraph] creates a rectangle node with text [&]*)
let nandNode ?(custom=default_informations) kgraph =
	let node = opNode ~custom:custom kgraph "&amp;" 0.5 0.6 in
	node

(** [triangle ()] creates a container that has the shape of a triangle*)
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

(** [regNode kgraph] creates a rectangle with a triangle at the bottom. *)
let regNode ?(custom=default_informations) kgraph =
	let node = defaultNode kgraph in
	resetPortsSurrounding node;
	let cont = opContWtT () in
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

(** [bufferNode kgraph] creates a rectangular node with text [1] *)
let bufferNode ?(custom=default_informations) kgraph = 
	opNode ~custom:custom kgraph "1" 0.5 0.6

(** Creates a polygon that has the shape for a mux*)
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

(** Creates a container Rendering that has the shape of a mux *)
let muxShape ?(custom=default_informations) () = 
	let cont = opContWtT ~custom:custom () in
	cont#setContainer (muxContainer ());
	cont

(** [muxNode kgraph] creates an node with mux rendering, default height is [30]*)
let muxNode ?(custom=default_informations) kgraph = 
	let node = defaultNode kgraph in
	let cont = muxShape ~custom:custom () in
	node#setHeight 30.0;
	node#addData cont;
	resetPortsSurrounding node;	
	node#addProperty (PersistentEntry.nodeSize "[NODE_LABELS, PORTS, PORT_LABELS, MINIMUM_SIZE]");	
	node

(** [condNode kgraph] creates a node with mux shape , default height is [40]*)
let condNode ?(custom=default_informations) kgraph = 
	let cont = muxShape ~custom:custom () in
	let node = defaultNode ~order:true kgraph in
	node#setHeight 40.0;
	node#addData cont;
	node#addProperty (PersistentEntry.nodeSize "[NODE_LABELS, PORTS, PORT_LABELS, MINIMUM_SIZE]");
	resetPortsSurrounding node;
	node

(** [matchNode kgraph] creates a node with mux shape *) 
let matchNode ?(custom=default_informations) kgraph = 
	let cont = muxShape ~custom:custom () in
	let node = defaultNode kgraph in
	node#setHeight 40.0;
	node#addData cont;
	node#addProperty (PersistentEntry.nodeSize "[NODE_LABELS, PORTS, PORT_LABELS, MINIMUM_SIZE]");
	node#addProperty (PersistentEntry.addPortSpace "0.0" "0.0" "10.0" "10.0");
	node

(** [bufferNode kgraph] creates a rectangular node with text [()] *)
let tupleNode ?(custom=default_informations) kgraph = 
	opNode ~custom:custom kgraph "()" 0.5 0.5 

(** [constNode kgraph s] creates a node with shape rectangular which right border is a triangle, with text [s].
	If option [const] is [false] (default [true]), the node is put left on the layout. *)
let constNode ?(custom=default_informations) ?(const=true) kgraph text = 
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
	
	let cont = opContWtT ~custom:custom () in
	cont#setContainer d;
	cont#addContainerRendering (textContainer ~custom:custom ~ho_mar:(5.0) text 0.5 0.5);
	node#addData cont;	
	node

(** [inputVarNode kgraph s] creates a node with same shape as constNode, with text [s]. It is put on the left on the layout. *)
let inputVarNode ?(custom=default_informations) kgraph text = 
	constNode ~custom:custom ~const:false kgraph text

(** [sinkNode kgraph s] creates an node with rectangular shape, with left border as a triangle, with text [s]. 
	Option [used] (default [true]) states if the node should be put on the right of the layout. *)
let sinkNode ?(custom=default_informations) ?(used=true) kgraph text = 
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

	let cont = opContWtT ~custom:custom () in
	let node = defaultNode ~layer:(if used then "LAST" else "NONE") kgraph in
	cont#setContainer d;
	cont#addContainerRendering (textContainer ~custom:custom ~ho_mar:(5.0) text 0.5 0.5);
	node#addData cont;
	resetPortsSurrounding node;
	node

(** [fbyNode kgraph] creates a node with rectangular shape and text [fby] *)
let fbyNode ?(custom=default_informations) kgraph =
	opNode ~custom:custom ~ho_mar:5.0 kgraph "fby" 0.5 0.5 

(** [textNode kgraph text] creates a node with rectangular shape and text [text] *)
let textNode ?(custom=default_informations) kgraph text = 
	opNode ~custom:custom ~ho_mar:5.0 kgraph text 0.5 0.5

(* for z *)

(** [addNode kgraph] creates a node with rectangular shape and text [+] *)
let addNode ?(custom=default_informations) kgraph = 
	opNode ~custom:custom kgraph "+" 0.5 0.5

(** [minusNode kgraph] creates a node with rectangular shape and text [-] *)
let minusNode ?(custom=default_informations) kgraph = 
	opNode ~custom:custom ~order:true kgraph "-" 0.5 0.5

(** [timesNode kgraph] creates a node with rectangular shape and text [*] *)
let timesNode ?(custom=default_informations) kgraph = 
	opNode ~custom:custom kgraph "*" 0.5 0.6

(** [divNode kgraph] creates a node with rectangular shape and text [/] *)
let divNode ?(custom=default_informations) kgraph = 
	opNode ~custom:custom ~order:true kgraph "/" 0.5 0.5

(** [lastNode kgraph] creates a node with rectangular shape and text [last] *)
let lastNode ?(custom=default_informations) kgraph = 
	opNode ~custom:custom kgraph ~ho_mar:(5.0) "last" 0.5 0.5

(** [constrBox cont rightDelta high] creates a filled point of diameter [5.0], with a dashed rectangle around it of dimension 10.0, 
with an outgoing line from the point heading to the north if [high] is [true], else to the east. 
This rendering is placed at the given [rightDelta] position from the east border

Option [no_line] to not draw the outgoing line (default [false]) *)
let constrBox ?(custom=default_informations) ?(no_line=false) (cont : containerRendering) rightDelta high =
	(* creation of the point*)
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
	
	(* creation of the dashed rectangle*)
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
	
	(* creation of the line*)
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

(** deConstrNode kgraph name num creates a rectangular node with first text [name], then [num] [constrBoxes] where the last is heading to the right *)
let deConstrNode ?(custom=default_informations) ?(vert=false) ?(right_ofs=27.35) kgraph name num = 
	let node = defaultNode ~order:true kgraph in
	let cont = opContWtT ~custom:custom () in
	let rec addBox n = match n with
		| -1 -> ()
		| _ -> 
			constrBox ~custom:custom cont (1.0 +. 3.0 *. (float_of_int n)) (vert || n <> 0);
			addBox (n - 1)
	in
	addBox (num - 1);
	let num = float_of_int num in
	
	let t = textContainer ~s:8 ~custom:custom name 0.5 0.5 in
	t#setPlacement (place ~top:0.0 ~left:5.0 ~right:(1.0 +. 3.0 *.num *. 5.0 +. 5.0) ~bottom:0.0 ());	
	cont#addContainerRendering t;

	node#addData cont;

	node#addProperty (PersistentEntry.create_property "org.eclipse.elk.spacing.individual" 
	("org.eclipse.elk.spacing.portPort:15.0;,;org.eclipse.elk.spacing.portsSurrounding:[top=0.0,left=0.0,bottom=0.0,right="^(string_of_float right_ofs)^"]"));

	node#addProperty (PersistentEntry.create_property "org.eclipse.elk.portAlignment.north" "END");
	node

(** [deConstrNode kgraph name num] creates a rectangular node with first text [name], then [num] [constrBoxes] where all are vertical. *)
let constrNode ?(custom=default_informations) kgraph name num =
	deConstrNode ~custom:custom ~vert:true ~right_ofs:12.35 kgraph name num

(** [integrCont alone] creates a container consisting of an horizontal followed by an horizontal line to the top and lastly an horizontal line in an *)
let integrCont ?(custom=default_informations) alone = 
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
	let c2 = if alone then Point.create_coord ~pos_val:(Rel 0.5) Top 
		else Point.create_coord ~pos_val:(Abs 10.0) Top in
	let point = Point.create_point c1 c2 in
	pd#setRefPoint point;
	line#setPlacement (Point pd);
	line	


let derNode ?(custom=default_informations) kgraph init_name =
	let node = defaultNode ~order:true kgraph in
	node#setWidth 30.0;
	node#setHeight 30.0;
	let cont = opContWtT ~custom:custom () in
	cont#addContainerRendering (integrCont ~custom:custom (init_name = ""));
	let t = textContainer ~s:8 ~custom:custom init_name 0.5 0.5 in
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
	let cont = opContWtT ~spe_back:(Some (cond_color ())) ~custom:custom () in
	cont#setContainer (Polygon [p1;p2;p3;p4;p1]);
	cont

let scondNode ?(custom=default_informations) kgraph title = 
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


let invisibleNode ?(custom=default_informations) kgraph =
	let node = defaultNode ~order:true kgraph in
	
	let cont = opContWtT ~inv:true ~custom:custom () in
	node#addData cont;
	addPortSpace node;
	
	node

let presentNode ?(custom=default_informations) kgraph = 
	let node = defaultNode kgraph in
	let cont = opContWtT ~spe_back:(Some (fct_color custom#getLayer)) ~custom:custom () in
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

let resetDerNode ?(custom=default_informations) kgraph = 
	let node = defaultNode kgraph in
	let cont = opContWtT ~spe_back:(Some (fct_color custom#getLayer)) ~custom:custom () in
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


let periodNode ?(custom=default_informations) kgraph = 
	opNode ~custom:custom ~order:true ~ho_mar:5.0 kgraph "Period" 0.5 0.5

let emitNode ?(custom=default_informations) kgraph name = 
	let node = defaultNode ~order:true kgraph in
	let cont = opContWtT ~custom:custom () in
	let t = textContainer ~custom:custom "emit" 0.5 0.5 in
	let botPlace = if name <> "" then 12.0 else 0.0 in
	t#setPlacement (place ~top:0.0 ~left:5.0 ~right:5.0 ~bottom:botPlace ());	
	cont#addContainerRendering t;
	let t = textContainer ~custom:custom name 0.5 0.5 in
	t#setPlacement (place ~top:12.0 ~left:5.0 ~right:5.0 ~bottom:0.0 ());
	cont#addContainerRendering t;
	node#addData cont;
	resetPortsSurrounding node;
	node

let upNode ?(custom=default_informations) kgraph = 
	opNode ~custom:custom ~ho_mar:5.0 kgraph "Up" 0.5 0.5

let blanckNode ?(custom=default_informations) kgraph portPos =
	let order = portPos = ORDER in
	let side = portPos <> NONE in
	let node = defaultNode ~side:side ~order:order kgraph in
	let cont = opContWtT ~spe_back:(Some (fct_color custom#getLayer)) ~custom:custom () in
	node#addData cont;
	addPortSpace node;
	cont#addAction (Actions.create_actionCollapse ());
	node#addProperty (PersistentEntry.nodeSize "[NODE_LABELS, PORTS, PORT_LABELS, MINIMUM_SIZE]");	
	node

let minusGreaterNode ?(custom=default_informations) kgraph = 
	opNode ~custom:custom ~ho_mar:5.0 kgraph "-&gt;" 0.5 0.5


let nextNode ?(custom=default_informations) kgraph name = 
	let node = defaultNode ~order:true kgraph in
	let cont = opContWtT ~custom:custom () in
	let t = textContainer ~custom:custom "next" 0.5 0.5 in
	let botPlace = if name <> "" then 12.0 else 0.0 in
	t#setPlacement (place ~top:0.0 ~left:5.0 ~right:5.0 ~bottom:botPlace ());	
	cont#addContainerRendering t;
	let t = textContainer ~custom:custom name 0.5 0.5 in
	t#setPlacement (place ~top:12.0 ~left:5.0 ~right:5.0 ~bottom:0.0 ());
	cont#addContainerRendering t;
	node#addData cont;
	resetPortsSurrounding node;
	node


let innerRecordNode ?(custom=default_informations) kgraph name = 
	let node = defaultNode kgraph in
	let cont = new containerRendering in
	cont#setContainer Rect;
	cont#addStyle (create_style (LineWidth 0.0));
	cont#addContainerRendering (textContainer ~custom:custom name 0.5 0.5);
	node#addData cont;
	resetPortsSurrounding node;
	node


let recordNode ?(custom=default_informations) kgraph = 
	let node = defaultNode ~order:true kgraph in	
	let cont = opContWtT ~custom:custom () in
	node#addProperty (PersistentEntry.create_property "org.eclipse.elk.direction" "UP");
	node#addData cont;
	node

let initNode ?(custom=default_informations) kgraph name = 
	let node = defaultNode ~order:true kgraph in
	let cont = opContWtT ~custom:custom () in
	if name <> "" then begin
		let t = textContainer ~custom:custom "init" 0.5 0.5 in
		t#setPlacement (place ~top:0.0 ~left:5.0 ~right:5.0 ~bottom:12.0 ());	
		cont#addContainerRendering t;
		let t = textContainer ~custom:custom name 0.5 0.5 in
		t#setPlacement (place ~top:12.0 ~left:5.0 ~right:5.0 ~bottom:0.0 ());
		cont#addContainerRendering t;
	end else begin
		let t = textContainer ~custom "init" 0.5 0.5 in
		t#setPlacement (place ~top:0.0 ~left:5.0 ~right:5.0 ~bottom:0.0 ());
		cont#addContainerRendering t;
	end;
	node#addData cont;
	resetPortsSurrounding node;
	node

let discNode ?(custom=default_informations) kgraph = 
	opNode ~custom:custom ~ho_mar:5.0 kgraph "disc" 0.5 0.5

let testNode ?(custom=default_informations) kgraph = 
	opNode ~custom:custom ~ho_mar:5.0 kgraph "?" 0.5 0.5

let invStateNode (kgraph : kgraph) = 
	let node = new knode kgraph in
	let cont = new containerRendering in
	node#setWidth 1.0;
	node#setHeight 1.0;
	cont#addStyle (create_style (Invisibility));
	node#addProperty (PersistentEntry.layerConstraint "FIRST_SEPARATE");
	node#addData cont;
	node

let appNode ?(custom=default_informations) kgraph = 
	opNode ~custom:custom ~ho_mar:5.0 kgraph "App" 0.5 0.5

let arrowBox ?(custom=default_informations) ?(dim=5.0) (cont : containerRendering) rightDelta = 
	let t = textContainer ~custom:custom ~s:8 "-" 0.5 0.5 in
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
	
	let t = textContainer ~custom:custom ~s:8 "&gt;" 0.5 0.5 in
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
	let cont = opContWtT ~custom:custom () in
	let rec addBox n = match n with
	| -1 -> ()
	| _ -> 
		constrBox ~custom:custom ~no_line:(n < num - num_taken) cont (2.5 +. 4.5 *. (float_of_int n)) true;
		arrowBox ~custom:custom cont (1.5 +. 4.5 *. (float_of_int n));
		addBox (n-1)
	in
	addBox (num - 1);
	
	let num = (float_of_int num) in
	let t = textContainer ~s:8 ~custom:custom name 0.5 0.5 in
	t#setPlacement (place ~top:0.0 ~left:5.0 ~right:((2. +. 4.5 *. num) *. 5.0) ~bottom:0.0 ());
	cont#addContainerRendering t;
	node#addData cont;
	
	let right_ofs = 19.85 +. 22.5 *. (num -. (float_of_int num_taken)) in

	node#addProperty (PersistentEntry.create_property "org.eclipse.elk.spacing.individual"
	("org.eclipse.elk.spacing.portPort:22.5;,;org.eclipse.elk.spacing.portsSurrounding:[top=0.0,left=0.0,bottom=0.0,right="^(string_of_float right_ofs)^"]"));
	node#addProperty (PersistentEntry.create_property "org.eclipse.elk.portAlignment.north" "END");

	node


let addEnoughSize (node : Knode.knode) = 
	node#addProperty (PersistentEntry.nodeSize "[NODE_LABELS, PORTS, PORT_LABELS, MINIMUM_SIZE]")


let emptyNode ?(custom=default_informations) (kgraph : Kgraph.kgraph) =
	let node = new knode kgraph in
	resetPortsSurrounding node;
	node#setHeight 0.1;
	node#setWidth 0.1;
	let cont = new containerRendering in

	let c1 = create_coord Left in
	let c2 = create_coord ~pos_val:(Rel 0.5) Top in
	let c3 = create_coord Right in
	let p1 = create_point c1 c2 in
	let p2 = create_point c3 c2 in
	
	cont#setContainer (Polygon [p1;p2]);
	cont#addStyle (create_style (Background (create_coloring (opLineColor custom))));
	cont#addStyle (create_style (Foreground (create_coloring (opLineColor custom))));
	node#addData cont;
	node
(* end for z *)

let function_node ?(always_expand=false) ?(order=false) ?(custom=default_informations) ?(res=false) ?(aut=false) ?(m=false) kgraph name layer = 
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
	
	main_node#addProperty (PersistentEntry.expand (string_of_bool (!InterLib_options.do_show_all || (layer=0)|| always_expand)));
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

let syncNode ?(custom=default_informations) ?(init=false) kgraph =
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

let seqBlockNode ?(custom=default_informations) kgraph name = 
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


let bubleNode ?(init=false) kgraph = 
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
	let t = textContainer ~s:8 "Pause" 0.5 0.5 in
	let white = create_color 255 255 255 in
	t#addStyle (create_style (Foreground (create_coloring white)));
	cont#addContainerRendering t;
	node#addData cont;
	node


let ramNode ?(custom=default_informations) kgraph = 
	let node = opNode ~custom:custom ~title:true ~order:true kgraph "ram" 0.5 0.1 in
	node#setHeight 130.0;
	node#setWidth 70.0;
	node#addProperty (PersistentEntry.portLabelPlacement "[INSIDE,NEXT_TO_PORT_IF_POSSIBLE]");
	node

let romNode ?(custom=default_informations) kgraph = 
	let node = opNode ~custom:custom ~center:true ~title:true ~order:true kgraph "rom" 0.5 0.5 in
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

	let h = textContainer ~s:7 "H" 0.5 0.5 in

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

let labelOfEdgeLabel ?(custom=default_informations) (kgraph : Kgraph.kgraph) name pos = 
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

let inlinedLabel ?(custom=default_informations) (kgraph : Kgraph.kgraph) name =
	let label = new Klabel.klabel kgraph in
	label#setText name;
	let cont = new containerRendering in
	let color = create_color 240 240 240 in
	cont#setContainer (RoundRect (8.0,8.0));
	cont#addStyle (create_style (LineStyle DASH));
	cont#addStyle (create_style (Background (create_coloring color)));
	cont#addContainerRendering (textContainer ~ho_mar:5.0 name 0.5 0.5);
	label#addData cont;
	label#addProperty (PersistentEntry.create_property "org.eclipse.elk.edgeLabels.inline" "true");
	label

let portLabel ?(custom=default_informations) (kgraph : Kgraph.kgraph) name = 
	let label = new Klabel.klabel kgraph in
	label#setText name;
	let cont = new containerRendering in
	cont#setContainer (Text name);
	cont#addStyle (create_style (Foreground (create_coloring (opLineColor custom))));
	label#addData cont;
	label

let seq_edge ?(half=false) ?(sourcePort=None) ?(targetPort=None) (kgraph : Kgraph.kgraph) source target = 
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

let automaton_edge ?(custom=default_informations) ?(sourcePort=None) ?(targetPort=None) (kgraph : Kgraph.kgraph) e_type source target = 
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

let lineStyle ty = 
	match ty with
	| Dash -> DASH
	| Dot -> DOT
	| _ -> SOLID

let new_edge ?(custom=default_informations) ?(edge_type=Simple) ?(thick=Gu_Simple) (kgraph : Kgraph.kgraph) sourceNode sourcePort targetNode targetPort = 
	let edge = new kedge kgraph in
	edge#setSource sourceNode;
	edge#setSourcePort sourcePort;
	edge#setTarget targetNode;
	edge#setTargetPort targetPort;
	let cont = new containerRendering in
	cont#setContainer (PolyLine []);
	
	cont#addStyle (create_style (LineWidth (lineThickness thick)));
	cont#addStyle (create_style ~on_sel:true (LineWidth (lineSelThickness thick)));
	if (lineStyle edge_type <> SOLID)  then cont#addStyle (create_style (LineStyle (lineStyle edge_type)));
	let color = create_color 100 100 255 in
	cont#addStyle (create_style (Foreground (create_coloring (opLineColor custom))));
	cont#addStyle (create_style ~on_sel:true (Foreground (create_coloring color)));
	cont#addJunction (junction ~custom:custom thick);
	edge#addData cont;
	edge

let addDirectionPriority (kedge : Kedge.kedge) prio = 
	if prio <> 0 then kedge#addProperty (PersistentEntry.create_property "org.eclipse.elk.layered.priority.direction" (string_of_int prio))

let addInsideSelfEdge (kedge : Kedge.kedge) value = 
	if value then kedge#addProperty (PersistentEntry.create_property "org.eclipse.elk.insideSelfLoops.yo" "true")

let addInsideSelfNode (knode :Knode.knode) value = 
	if value then knode#addProperty (PersistentEntry.create_property "org.eclipse.elk.insideSelfLoops.activate" "true")

let linkEdge ?(custom=default_informations) (kgraph : Kgraph.kgraph) sourceNode sourcePort targetNode targetPort =
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
