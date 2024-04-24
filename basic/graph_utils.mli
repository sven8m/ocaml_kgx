(**type for edge endpoint *)
type endPoint = {
	node : ObjectSig.knodeSig;
	port : ObjectSig.kportSig;
	var : bool; (**if the endpoint corresponds to a variable or not *)
	mutable labels : string list; (**labels to draw on edge *)
}

(**type for a graphical operation node *)
type op_node = {
	node : ObjectSig.knodeSig; (**graphical knode *)
	inner_inputs : endPoint list; (**input ports, placed left, for the inside of the operation node *)
	outer_inputs : endPoint list; (**input ports, placed left, for the incoming inputs *)
	inner_outputs : endPoint list; (**output ports, placed right , for the outputs comming from inside the operation node*)
	outer_outputs : endPoint list; (**output ports, placed right, used outside the operation node *)
	control : endPoint list; (**control ports, placed top or down *)
}

val place :
	?top:float -> ?left:float -> ?right:float -> ?bottom:float -> unit -> Placement.placement
(** creates area placement with optional offset from the boundaries. 
	Default is top=15.0, left=right=bottom=10.0 *)

val absolutePointPlacement :
	?minW:float -> ?minH:float -> float -> float -> Placement.placement
(** [absolutePointPlacement x y] creates a point placement where the reference point (x,y) is absolute from top left 
	Optional is given the minimum width or height for the object *)

val centerPlacement :
	?ho_mar:float option -> ?ver_mar:float option -> float -> float -> Placement.placement
(** [centerPlacement x y] creates a point placement where the reference point (x,y) is relative from top left
	Optional is given an horizontal or vertical margin *)

val defaultNode : ?layer:string -> ?order:bool -> Kgraph.kgraph -> Knode.knode
(** Creates a default node with width and height 20 and port side constraint.
	Option [order] if the port order should be considered.
	Option [layer] with value  "LAST", "FIRST" or "NONE" if the node should be placed left or right of the layout. *)

val defaultStateNode : ?layer:string -> Kgraph.kgraph -> Knode.knode
(** Creates a default node, as for defaultNode, but with no port side constraint. *)

val simpleOpContWtT : unit -> Rendering.containerRendering
(** Creates a simple rectangular container *)

val simpleOpNodeWtT : Kgraph.kgraph -> Knode.knode
(** Creates a simple node for operations consisting of a [defaultNode] and a [simpleOpContWtT] *)

val functionTitle :
	?center:bool -> ?ho_mar:float -> string -> Rendering.containerRendering
(** [functionTitle s] Creates a text rendering with text s that is a function title. 
	The text is centered from the left and 15 from the top.
	Option [center] if the text should be centered from the top.
	Option [ho_mar] to put an horizontal margin. *)

val simpleText : 
	?s:int -> ?ho_mar:float -> ?ver_mar:float -> string -> float -> float -> Rendering.containerRendering
(** [simpleText s x y] creates a text rendering with text s and a centered placement from (x,y).
	Option [s] to change the font size (default 11)
	Options [ho_mar] and [ver_mar] to put a vertical and horizontal margin (default 0) *)

val invisiblePort : Kgraph.kgraph -> Knode.knode -> Kport.kport
(** [invisiblePort kgraph node] creates a port linked to the [node] that is invisible. *)

val invisibleOutputPort : Kgraph.kgraph -> Knode.knode -> Kport.kport
(** [invisibleOutputPort kgraph node] creates an invisible port linked to the [node] on the eastern side. *)

val invisibleInputPort : Kgraph.kgraph -> Knode.knode -> Kport.kport
(** [invisibleInputPort kgraph node] creates an invisible port linked to the [node] on the western side. *)

val invisibleControlPort : ?ofs:float -> Kgraph.kgraph -> Knode.knode -> Kport.kport
(** [invisibleControlPort kgraph node] creates an invisible port linked to the [node] on the north or south side. *)

val visibleControlPort : ?ofs:float -> Kgraph.kgraph -> Knode.knode -> Kport.kport
(** [visibleControlPort kgraph node] creates a port linked to the [node] on the north or south side. It is a 5*5 black box.
	Option [ofs] gives a border offset. *)

val visiblePort : Kgraph.kgraph -> Knode.knode -> Kport.kport
(** [visiblePort kgraph node] creates a port linked to the [node] that is visible (5*5 black box) *)

val visibleOutputPort : Kgraph.kgraph -> Knode.knode -> Kport.kport
(** [visibleOutputPort kgraph node] creates a visible port linked to the [node] on the eastern side. *)

val visibleInputPort : Kgraph.kgraph -> Knode.knode -> Kport.kport
(** [visibleInputPort kgraph node] creates a visible port linked to the [node] on the western side. *)

val notOutputPort : Kgraph.kgraph -> Knode.knode -> Kport.kport
(** [notOutputPort kgraph node] creates a port kinked to the [node] on the eastern side, that is a circle. *)

val multOpNode : ?no:bool -> Kgraph.kgraph -> Knode.knode -> int -> op_node
(** [multOpNode kgraph node n] creates an op_node with node [node], [n] input ports and an output port.
	Option [no] to spectify if the output port is a notOutputPort. *)

val binopNode : ?no:bool -> Kgraph.kgraph -> Knode.knode -> op_node
(** [binopNode kgraph node] creates an op_node with node [node], 2 input ports and an output port.
	Option [no] to spectify if the output port is a notOutputPort. *)

val unopNode : ?no:bool -> Kgraph.kgraph -> Knode.knode -> op_node
(** [unopNode kgraph node] creates an op_node with node [node], 1 input port and an output port.
	Option [no] to spectify if the output port is a notOutputPort. *)
	
val resetPortsSurrounding : Knode.knode -> unit
(** [resetPortsSurrounding node] resets the additional port space for this node.*)

val addPortSpace : Knode.knode -> unit
(** [addPortSpace node] add a port space for the children , 12 from top, 0 from bottom and 10 from left and right. *)

val simpleOpNode :
	?center:bool ->
	?title:bool ->
	?ho_mar:float ->
	?order:bool -> Kgraph.kgraph ->	string -> float -> float -> Knode.knode
(** [simpleOpNode s x y] creates a node for simple operations, with text s positioned centered at relative x and y.
	Option [center] to use with option [title] to center the function title.
	Option [title] to specify if the text should be a function title.
	Option [ho_mar] to specify if the text should have an horizontal margin.
	Option [order] to specify that the ports have fixed order.*)

val simpleXorNode : Kgraph.kgraph -> Knode.knode
(** [simpleXorNode kgraph] creates a rectangle with text [=1]*)

val simpleAndNode : Kgraph.kgraph -> Knode.knode
(** [simpleAndNode kgraph] creates a rectangle with text [&] *)

val simpleOrNode : Kgraph.kgraph -> Knode.knode
(** [simpleOrNode kgraph] creates a rectangle with text [>=1] *)

val simpleSelectNode : Kgraph.kgraph -> string -> Knode.knode
(** [simpleSelectNode kgraph i] creates a rectangle with text [i]. *)

val simpleSliceNode : Kgraph.kgraph -> string -> string -> Knode.knode
(** [simpleSliceNode kgraph i j] creates a rectangle with text [i..j] .*)

val simpleConcatNode : Kgraph.kgraph -> Knode.knode
(** [simpleConcatNode kgraph] creates a rectangle with text [.]  *)

val rectangleAsPolygon : unit -> Container.container
(** [rectangleAsPolygon ()] creates a container that has the form of a rectangle but is formed by a polygon. *)

val simpleNandNode : Kgraph.kgraph -> Knode.knode
(** [simpleNandNode kgraph] creates a node with text [&] *)

val triangle : unit -> Container.container
(** [triangle ()] creates a container that has the shape of a triangle*)

val simpleRegNode : Kgraph.kgraph -> Knode.knode
(** [simpleRegNode kgraph] creates a rectangle with a triangle at the bottom. *)

val simpleBufferNode : Kgraph.kgraph -> Knode.knode
(** [simpleBufferNode kgraph] creates a rectangle with text [1] *)

val simpleNotNode : Kgraph.kgraph -> Knode.knode
(** [simpleNotNode kgraph] creates a rectangle with text [1] *)

val muxContainer : unit -> Container.container
(** Creates a polygon that has the shape for a mux*)

val simpleMuxShape : unit -> Rendering.containerRendering
(** Creates a container Rendering that has the shape of a mux *)

val simpleMuxNode : Kgraph.kgraph -> Knode.knode
(** [simpleMuxNode kgraph] creates an node with mux shape *)

val simpleCondNode : Kgraph.kgraph -> Knode.knode
(** [simpleCondNode kgraph] creates a node with mux shape *)

val simpleMatchNode : Kgraph.kgraph -> Knode.knode
(** [simpleMatchNode kgraph] creates a node with mux shape *) 

val simpleTupleNode : Kgraph.kgraph -> Knode.knode
(** [simpleTupleNode kgraph] creates a node with text [()] *)

val simpleUnTupleNode : Kgraph.kgraph -> Knode.knode
(** [simpleUnTupleNode kgraph ] creates a node with text [()] *)

val simpleConstNode : ?const:bool -> Kgraph.kgraph -> string -> Knode.knode
(** [simpleConstNode kgraph s] creates a node with shape rectangular which right border is a triangle, with text [s].
	If [const] is [false] (default [true]), the node is put left on the layout. *)

val simpleInputVarNode : Kgraph.kgraph -> string -> Knode.knode
(** [simpleInputVarNode kgraph s] creates a node with same shape as constNode, with text [s]. It is put on the left on the layout. *)

val simpleSinkNode : ?used:bool -> Kgraph.kgraph -> string -> Knode.knode
(** [simpleSinkNode kgraph s] creates an node with rectangular shape, with left border as a triangle, with text [s]. 
	Option [used] (default [true]) states if the node should be put on the right of the layout. *)

val simpleFbyNode : Kgraph.kgraph -> Knode.knode
(** [simpleFbyNode kgraph] creates a node with rectangular shape and text [fby] *)

(*
val createPort : Kgraph.kgraph -> Knode.knode -> string -> Kport.kport
(** [createPort kgraph node s] creates a basic port with label [s] and linked to the node [node].*)
*)

val layered_color : int -> int -> int -> int -> Coloring.color
(** [layered_color red blue green layer] creates a color depending on the [layer].*)

val aut_color : unit -> Coloring.color
(** Creates the background color of an automaton *)

val fct_color : int -> Coloring.color
(** [fct_color layer] creates the background color of a function node depending on the [layer]*)

val match_color : int -> Coloring.color
(** [match_color layer] creates the background color of a match node depending on the [layer]*)

val reset_color : int -> Coloring.color
(** [reset_color layer] creates the background color of a reset node depending on the [layer]*)

val function_node : ?res:bool -> ?aut:bool -> ?m:bool -> Kgraph.kgraph -> string -> int -> Knode.knode
(** [function_node kgraph name layer] creates a function node with function title [name] and the background depending on the [layer].
	Option [res] (default [false]) indicates if the node is a reset node (adequate color)
	Option [aut] (default [false])  indicates if the node is an automaton node (adequate color)
	Option [m] (default [false])  indicates if the node is a match node (adequate color)*)

val functionReset : ?res:bool -> ?m:bool -> Kgraph.kgraph -> string -> int -> Knode.knode
(** Same as [function_node], but for nodes which will have a reset port. *)

(*
val addReset : Kgraph.kgraph -> op_node -> Knode.knode
(** [addReset kgraph opNode] adds a control port to the node corresponding to opNode *)
*)
val state_color : int -> Coloring.color
(** [state_color layer] creates the background color of a state node depending on the [layer]*)

val stateNode : ?init:bool -> Kgraph.kgraph -> string -> int -> Knode.knode
(** [stateNode kgraph s layer] creates a state node with title name [s] and background color depending on [layer].
	Option [init] indicates if the node is initial (default [false]). Change the line size adequately *)

val sync_color : int -> Coloring.color
(** [sync_color layer] creates the color of a sync node depending on the [layer]*)

val terminalSyncNode : Kgraph.kgraph -> Knode.knode
(** Creates the terminal Sync node.*)

val simpleSyncNode : ?init:bool -> Kgraph.kgraph -> Knode.knode
(** [simpleSyncNode kgraph] creates a node for sync parts.
	Option [init] indicates if the node is initial. (default [false])*)

val simpleSeqBlockNode : Kgraph.kgraph -> string -> Knode.knode
(** [simpleSeqBlockNode kgraph s] creates a node for a sequential bloc, with name [s]. *)

val simpleBubleNode : ?init:bool -> Kgraph.kgraph -> Knode.knode
(** [simpleBubleNode kgraph] creates a black ellipse node with size 30.
	Option [init] changes the size to 40*)

val ramNode : Kgraph.kgraph -> Knode.knode
(** [ramNode kgraph] creates a ram node with title [ram]. *)

val romNode : Kgraph.kgraph -> Knode.knode
(** [romNode kgraph] creates a rectangular node with title [rom]. *)	

val junction : bool -> Rendering.containerRendering
(** [junction mult] creates a junction buble of size 4, 5 if [mult] is true (default [false])*)

val arrow_decorator : ?h:bool -> bool -> Rendering.containerRendering
(** [arrow_decorator alone] creates a container rendering having the shaped of an arrow with decorator placement at the end of a line,
	change if [alone] is false. Option [h] indicates that there will be a history bubble at the end of the line and the arrow decorator is
	moved accordinely *)
	
val red_dot : bool -> Rendering.containerRendering
(** [red_dot b] creates a decorator in the shape of red dot, placed at relative beginning if [b], else at the end*)

val arrow_red_dot : unit -> Rendering.containerRendering
(** [arrow_red_dot ()] creates a decorator formed by an arrow and a red dot *)

val history_dot : unit -> Rendering.containerRendering
(** [history_dot ()] creates a decorator that has a round shape with an H, placed at the relative end. *)

val arrow_history : unit -> Rendering.containerRendering
(** [arrow_history ()] creates a decorator with arrow and history dot, placed at relative end. *)

val green_triangle : bool -> Rendering.containerRendering
(** [green_triangle b] creates a decorator whose shape is a triangle, placed at the beginning iff [b] is true *)

val seq_edge : ?half:bool -> ?sourcePort:Kport.kport option -> ?targetPort:Kport.kport option ->
	Kgraph.kgraph  -> Knode.knode -> Knode.knode -> Intermediate_graph.edge_label list -> unit
(** [seq_edge kgraph source target lab] creates a directed edge from [source] to [target] with labels [lab] with
	shape rounded Polyline. 
	Option [half] (default [false]) to remove the arrow
	Option [sourcePort] and [targetPort] (default [None]) to specify a source or target port for the edge*)

val automaton_edge : Kgraph.kgraph -> Intermediate_graph.edge_type -> Knode.knode -> Knode.knode -> Intermediate_graph.edge_label list  -> unit
(** [automaton_edge e_type source target lab] creates a directed edge (with arrow decorator at the end) from [source] to [target] with labels [lab] with 
	shape spline. 
	If the type is [Aut_begin], then there is a red_dot at beginning.
	If the type is [Aut_end], then there is a red_dot at the end.
	If the type is [Aut_begin_history], then there is a red_dot at beginning and a history_dot at the end.
	If the type is [Aut_end_history], then there is a history_dot at the end. *)

val new_edge : ?mult:bool -> Kgraph.kgraph -> Knode.knode -> Kport.kport -> Knode.knode -> Kport.kport -> Intermediate_graph.edge_label list -> unit
(** [new_edge kgraph sourceNode sourcePort targetNode targetPort labels] creates a polyline from [sourceNode] with port [sourcePort] to [targetNode] with port [targetPort],
	with labels [labels] on the edge with position according to the second argument of the label.
	Option [mult] indicates that the line should be thicker. *)

val init_kgraph : unit -> Kgraph.kgraph
(** [init_kgraph ()] creates a kgraph *)

val main : 'a -> unit
(** For testing only*)
