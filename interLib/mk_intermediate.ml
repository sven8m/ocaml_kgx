open Intermediate_graph

(** Mk_intermediate creates iObjects *)

(** [create_n_ports n node ty] creates [n] ports on node [node] of type [ty]. 

Option [no] indicates if it is a [no] port, [vis] if the port is visible.*)
let rec create_n_ports ?(no=false) ?(vis=false) n node ty = 
	match n with
	| 0 -> []
	| n -> 
		let port = new iPort node in
		port#setType ty;
		port#setVisible vis;
		port#setNot no;
		node#addPort port;
		let outer = new iOuterPort port in
		outer :: (create_n_ports ~no:no ~vis:vis (n-1) node ty)

let rec create_n_ports_special ?(no=false) ?(vis=true) n node ty =
	match n with
	| 0 -> []
	| n -> 
		let port = new iPort node in
		port#setType (ty (n-1));
		port#setVisible vis;
		port#setNot no;
		node#addPort port;
		let outer = new iOuterPort port in
		outer :: (create_n_ports_special ~no:no ~vis:vis (n-1) node ty)

(** [outerToEndPoint outer] takes an iOuterPort and creates a corresponding iEndPoint *)
let outerToEndPoint o = 
	let endPoint = new iEndPoint (o#getNode) (o#getPort) in
	endPoint

(** [number_ports node_type] returns number of inputs, outputs and control ports for a node of type [node_type]. *)
let number_ports node_type = match node_type with
	| Concat | Nand  -> 2,1,0
	| Slice _ | Select _ | Reg | Buffer | Not -> 1,1,0
	| Fby -> 1,1,1
	| Mux -> 2,1,1
	| Cond n -> (n+1) , 1 , n (*to check*)
	| Match l -> List.length l , 1, 1
	| And n | Or n | Xor n | Tuple n -> n , 1 , 0
	| UnTuple n -> 1 , n , 0
	| Var _ | Const _ -> 0, 1 , 0
	| Sink _ -> 1,0,0
	| Rom -> 1,1,0
	| Ram -> 4,1,0
	| Pause _ -> 0,0,0
	| Sync _ -> 0,0,0
	| For -> 0,0,0
	| While -> 0,0,0
	| Final -> 0,0,0
	| Every _ | Fct _ | Match_node | Match_state _ | Reset | Aut | Aut_state _ -> assert false
	| Link  -> 1,1,0
	(* for z *)
	| Add n | Mult n -> n, 1, 0
	| Minus | Div -> 2,1,0
	| Last -> 1,1,0
	| Deconstr _ -> 1,1,0
	| Constr _ -> 0,1,0

let topOutputs node_type = match node_type with
	| Deconstr (_,n) -> (n-1)
	| _ -> 0

let topInputs node_type = match node_type with
	| Constr (_,n) -> n
	| _ -> 0

(** [is_output_not nt] return true if the node type [nt] is [Nand] or [Not] *)
let is_output_not node_type = match node_type with
	| Nand | Not -> true
	| _ -> false

(** [addOuterNames ol nl] iterates over [ol] and [nl], and for [outer] and [name], it adds the name [name] to the outer port [outer] *)
let addOuterNames outer_list name_list = 
	List.iter2 (fun outer name ->
		outer#getPort#setName name) outer_list name_list

(** [addOffsets ol ofsl] iterates over [ol] and [ofsl], and for [outer] and [ofs], it adds the offset [ofs] to the outer port [outer] *)
let addOffsets outer_list ofs_list = 
	List.iter2 (fun outer ofs ->
		outer#getPort#setOffset ofs) outer_list ofs_list

(** [addNames node node_type] adds names on the ports of [node], depending on the [node_type] *) 
let addNames node node_type = 
	match node_type with
	| Match l -> addOuterNames node#getInputs l;
		addOffsets node#getControl [-.3.0 -. 3.0 *. (float_of_int (List.length l))]
	| Mux -> addOuterNames node#getInputs ["0";"1"];
		addOffsets node#getControl [-3.5]
	| Cond n ->
		let conds = List.init n (fun i -> string_of_int (i+1)) in 
		let inputs = List.init (n+1) (fun i -> if i <> n then string_of_int (i+1) else "else") in
		addOuterNames node#getControl conds;
		addOuterNames node#getInputs inputs;
		let offsets = List.init n (fun i ->
			let i = i + 1 in
			let delta = if i >= 10 then
				8.0 -. 0.013 *. (float_of_int n)
			else 8.0 -. 0.045 *. (float_of_int n)
			in
			let ofs = if i >= 10 then (-5.0) -. 0.013 *. (float_of_int n) else 4.0 in
			(-.ofs -. delta *. (float_of_int (i - 1)))
		) in
		addOffsets node#getControl offsets
	| Ram -> 
		addOuterNames node#getInputs ["read_addr";"write?";"write_addr";"write_data"]
	| _ -> ()

(** [createLinkEdge sourcePort targetNode targetPort] creates an edge of some link type, going from sourcePort to endpoint (targetNode , targetPort) 

Option [dep] (default [true]) if the link should have dependency.

Option [aut] (default [false]) if it is a link in an automaton. *)
let createLinkEdge ?(dep=true) ?(aut=false) sourcePort targetNode targetPort = 
	let edge = new iEdge in
	let eT = match dep, aut with
	| true,true -> DepAutLink
	| true,false -> DepLink
	| false,true -> AutLink
	| _ -> Link
	in
	edge#setTarget targetNode;
	edge#setTargetPort targetPort;
	edge#setType eT;
	edge#setSource (sourcePort#getParent);
	edge#setSourcePort sourcePort;
	sourcePort#addEdge edge;
	targetPort#addBackEdge edge

(** [linkCreation node] creates links between the input,control and output ports of [node], depending on it type *)
let linkCreation node =
	let input_dep = match node#getType with
	| Fby | Reg -> false
	| _ -> true
	in	
	List.iter (fun outer_input ->
		List.iter (fun outer_output ->
			createLinkEdge ~dep:input_dep outer_input#getPort outer_output#getNode outer_output#getPort)
		node#getOutputs
	) node#getInputs;
	
	List.iter (fun outer_control ->
		List.iter (fun outer_output ->
			createLinkEdge outer_control#getPort outer_output#getNode outer_output#getPort)
		node#getOutputs
	) node#getControl

(** [simpleOpNode node_type parent layer] creates an iNode corresponding to the [node_type], with number of ports, names and innerLinks depending on the [node_type], and parent [parent]. The [layer] is used for coloring.*)
let simpleOpNode node_type parent layer =
	let nb_inputs,nb_outputs, nb_control = number_ports node_type in
	let node = new iNode in
	node#setType node_type;
	node#setLayer layer;
	node#addOutputList (create_n_ports (topOutputs node_type) node OutputTop);
	node#addInputList (create_n_ports (topInputs node_type) node InputTop);
	node#addInputList (create_n_ports nb_inputs node Input);
	node#addOutputList (create_n_ports ~no:(is_output_not node_type) nb_outputs node Output);
	node#addControlList (create_n_ports ~vis:true nb_control node Control);
	addNames node node_type;
	linkCreation node;
	
	parent#addChild node;
	node

type fctParent = Node of iNode
| Graph of iGraph

(** [simpleFunctionNode node_type input_names output_names parent layer] creates an iNode for a [node_type] function, with 
input ports having names [input_names], and output ports having names [output_names]. 

Option [control] if there should be a control port on the node *)
let simpleFunctionNode ?(control=false) node_type input_names output_names parent layer = 
	let node = new iNode in
	node#setType node_type;
	node#setLayer layer;
	node#addInputList (create_n_ports ~vis:true (List.length input_names) node Input);
	node#addOutputList (create_n_ports ~vis:true (List.length output_names) node Output);
	if control then node#addControlList (create_n_ports ~vis:true 1 node Control); 
	addOuterNames node#getInputs input_names;
	addOuterNames node#getOutputs output_names;
	begin match parent with
	| Node p -> p#addChild node;
	| Graph g -> g#addNode node;
	end;
	node

(** [addReset node] adds a port of type control to the [node] *)
let addReset node = 
	node#addControlList (create_n_ports ~vis:true 1 node Control)

(** [new_edge edge_type source target] creates an iEdge of type [type_edge] from the endpoint [source] to the endpoint [target] *)
let new_edge edge_type (source : iEndPoint) target = 
	let edge = new iEdge in
	edge#setType edge_type;
	edge#setTarget target#getNode;
	edge#setTargetPort target#getPort;
	edge#setSource source#getNode;
	edge#setSourcePort source#getPort;
	List.iter (fun lab ->
		lab#setPosition Tail;
		edge#addLabel lab
	) source#eatLabels;
	List.iter (fun lab ->
		lab#setPosition Head;
		edge#addLabel lab) target#eatLabels;
	(source#getPort)#addEdge edge;
	(target#getPort)#addBackEdge edge

(** [automaton_edge_type reset beginning] gives the type of the automaton edge depending on the reset or beginning *)
let automaton_edge_type reset beginning = 
	match reset , beginning with
	| true , true -> Aut_begin
	| true, false -> Aut_end
	| false , true -> Aut_begin_history
	| false, false -> Aut_end_history

let automaton_edge source target lab reset beginning = 
	let edge_type = automaton_edge_type reset beginning in
	let edge = new iEdge in
	edge#setType edge_type;
	edge#setTarget target;
	edge#setSource source;
	let label = new iEdgeLabel lab in
	label#setPosition Center;
	edge#addLabel label;
	source#addEdge edge;
	target#addBackEdge edge

let seq_edge ?(half=false) ?(sourcePort=None) ?(targetPort=None) sourceNode targetNode label = 
	let edge_type = if half then Seq_half else Seq in
	let edge = new iEdge in
	edge#setType edge_type;
	edge#setTarget targetNode;
	edge#setSource sourceNode;
	let label = new iEdgeLabel label in
	label#setPosition Center;
	edge#addLabel label;
	begin match targetPort with
	| None -> targetNode#addBackEdge edge
	| Some p -> 
		edge#setTargetPort p;
		p#addBackEdge edge
	end;
	match sourcePort with
	| None -> sourceNode#addEdge edge
	| Some (p : iPort) -> 
		edge#setSourcePort p;
		p#addEdge edge

let edgeLabel ?(pos=Undef) ?(forced=Undef) name = 
	let lab = new iEdgeLabel name in
	lab#setPosition pos;
	lab#setForcedPosition forced;
	lab

