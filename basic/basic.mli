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

val invisibleControlPort : Kgraph.kgraph -> Knode.knode -> Kport.kport
(** [invisibleControlPort kgraph node] creates an invisible port linked to the [node] on the north or south side. *)

val visibleControlPort : ?ofs:float -> Kgraph.kgraph -> Knode.knode -> Kport.kport
(** [visibleControlPort kgraph node] creates a port linked to the [node] on the north or south side. It is a 5*5 black box.
	Option [ofs] gives a border offset. *)
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

val simpleXorNode :
	?mult:int -> Kgraph.kgraph -> op_node
(** [simpleXorNode kgraph] creates an op_node corresponding to a xor node.
	Option [mult] to specify the number of input ports (default 2). *)

val simpleAndNode : ?mult:int -> Kgraph.kgraph -> op_node
(** [simpleAndNode kgraph] creates an op_node corresponding to an and node.
	Option [mult] to specify the number of input ports (default 2). *)

val simpleOrNode : ?mult:int -> Kgraph.kgraph -> op_node
(** [simpleOrNode kgraph] creates an op_node corresponding to an or node.
	Option [mult] to specify the number of input ports (default 2). *)

val simpleSelectNode : Kgraph.kgraph -> string -> op_node
(** [simpleSelectNode kgraph i] creates an op_node corresponding to a selection node on position [i]. *)

val simpleSliceNode : Kgraph.kgraph -> string -> string -> op_node
(** [simpleSliceNode kgraph i j] creates an op_node corresponding to a slice from i to j node.*)

val simpleConcatNode : Kgraph.kgraph -> op_node
(** [simpleConcatNode kgraph] creates an op_node corresponding to a concatenation node. *)

val rectangleAsPolygon : unit -> Container.container
(** [rectangleAsPolygon ()] creates a container that has the form of a rectangle but is formed by a polygon. *)

val simpleNandNode : Kgraph.kgraph -> op_node
(** [simpleNandNode kgraph] creates an op_node corresponding to a nand node. *)

val triangle : unit -> Container.container
(** [triangle ()] creates a container that has the shape of a triangle*)

val simpleRegNode : Kgraph.kgraph -> op_node
(** [simpleRegNode kgraph] creates an op_node corresponding to a reg node. *)

val simpleBufferNode : Kgraph.kgraph -> op_node
(** [simpleBufferNode kgraph] creates an op_node corresponding to a buffer node. *)

val simpleNotNode : Kgraph.kgraph -> op_node
(** [simpleNotNode kgraph] creates an op_node corresponding to a negation node. *)

val muxContainer : unit -> Container.container
(** Creates a polygon that has the shape for a mux*)

val simpleMuxShape : unit -> Rendering.containerRendering
(** Creates a container Rendering that has the shape of a mux *)

val simpleMuxNode : Kgraph.kgraph -> op_node
(** [simpleMuxNode kgraph] creates an op_node corresponding to a mux node. *)

val simpleCondNode : Kgraph.kgraph -> int -> op_node
(** [simpleCondNode kgraph n] creates an op_node corresponding to a conditional node with [n] conditions. *)

val simpleMatchNode : Kgraph.kgraph -> string list -> op_node
(** [simpleCondNode kgraph sl] creates an op_node corresponding to a match node where [sl] is the list of constructors that are matched. *)

val simpleTupleNode : Kgraph.kgraph -> int -> op_node
(** [simpleTupleNode kgraph n] creates an op_node corresponding to a tuple node with [n] inputs. *)

val simpleUnTupleNode : Kgraph.kgraph -> int -> op_node
(** [simpleUnTupleNode kgraph n] creates an op_node corresponding to an untuple node with [n] outputs. *)

val simpleConstNode : ?const:bool -> Kgraph.kgraph -> string -> op_node
(** [simpleConstNode kgraph s] creates an op_node corresponding to a constant node with text [s].
	Option [const] (default [true]) statues if the node should not be put left or do.*)

val simpleInputVarNode : Kgraph.kgraph -> string -> op_node
(** [simpleInputVarNode kgraph s] creates an op_node corresponding to a node for a variable [s] as input. It is put on the left on the layout. *)

val simpleSinkNode : ?used:bool -> Kgraph.kgraph -> string -> op_node
(** [simpleSinkNode kgraph s] creates an op_node corresponding to a node for a sink node with text [s]. 
	Option [used] states if the node should be put on the right of the layout. *)

val simpleFbyNode : Kgraph.kgraph -> op_node
(** [simpleFbyNode kgraph] creates an op_node corresponding to a followed-by node. *)

val createPort : Kgraph.kgraph -> Knode.knode -> string -> Kport.kport
(** [createPort kgraph node s] creates a basic port with label [s] and linked to the node [node].*)

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

val function_node : ?res:bool -> ?aut:bool -> ?m:bool -> Kgraph.kgraph -> string -> string list -> string list -> int -> op_node
(** [function_node kgraph name input_names output_names layer] creates a function node with function title [name], input ports labeled with [input_names],
	output ports labeled with [output_names], and the background depending on the [layer].
	Option [res] (default [false]) indicates if the node is a reset node (adequate color)
	Option [aut] (default [false])  indicates if the node is an automaton node (adequate color)
	Option [m] (default [false])  indicates if the node is a match node (adequate color)*)

val functionReset : ?res:bool -> ?m:bool -> Kgraph.kgraph -> string -> string list -> string list -> int -> op_node
(** Same as [function_node], but there is an additional control port. *)

val addReset : Kgraph.kgraph -> op_node -> op_node
(** [addReset kgraph opNode] adds a control port to the node corresponding to opNode *)

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

val ramNode : Kgraph.kgraph -> op_node
(** [ramNode kgraph] creates a ram node. *)

val romNode : Kgraph.kgraph -> op_node
(** [romNode kgraph] creates a rom node. *)	

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
	Kgraph.kgraph  -> Knode.knode -> Knode.knode -> string -> unit
(** [seq_edge kgraph source target lab] creates a directed edge from [source] to [target] with label [lab] with
	shape rounded Polyline. 
	Option [half] (default [false]) to remove the arrow
	Option [sourcePort] and [targetPort] (default [None]) to specify a source or target port for the edge*)

val automaton_edge : Kgraph.kgraph -> Knode.knode -> Knode.knode -> string -> bool -> bool -> unit
(** [automaton_edge source target lab res b] creates a directed edge from [source] to [target] with label [lab] with 
	shape spline and with red_dot at beginning if [b] is true else if [res] then red_dot at the end else history_dot at the end *)

val new_edge : ?mult:bool -> Kgraph.kgraph -> endPoint -> endPoint -> Kedge.kedge
(** [new_edge kgraph source target] creates a polyline from [source.node] with port [source.port] to [target.node] with port [target.port]
	Option [mult] indicates that the line should be thicker. *)

val init_kgraph : unit -> Kgraph.kgraph
(** [init_kgraph ()] creates a kgraph *)

val main : 'a -> unit
(** For testing only*)
