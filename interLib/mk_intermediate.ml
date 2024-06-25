open Intermediate_graph

(** Mk_intermediate creates iObjects *)

(** [create_n_ports n node ty] creates [n] ports on node [node] of type [ty]. 

Option [no] indicates if it is a [no] port, [vis] if the port is visible, [bub] if the port is a buble,
[question] if the port is a question mark. By default all options are [false]. *)
let rec create_n_ports (*?(question=false) ?(no=false) ?(vis=false) ?(bub=false)*) n node ty = 
	match n with
	| 0 -> []
	| n -> 
		let port = new iPort node in
		port#setType ty;
		(*port#setVisible vis;
		port#setNot no;
		port#setQuestion question;
		port#setBuble bub;*)
		node#addPort port;
		let outer = new iOuterPort port in
		outer :: (create_n_ports (*~question:question ~no:no ~vis:vis ~bub:bub*) (n-1) node ty)

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
	| Minus n -> n, 1, 0
	| Div -> 2,1,0
	| Last -> 1,1,0
	| Deconstr _ -> 1,1,0
	| Constr _ -> 0,1,0
	| Der (_,n) -> 1,1,n
	| VertText _ -> 0,0,0
	| Inv -> assert false
	| Present  -> assert false
	| Text (_,n) -> n,1,0 
	| Period b -> if b then 1,1,1 else 1,1,0
	| Emit _ -> 1,1,0
	| Up -> 1,1,0
	| Scond _ -> assert false
	| BlanckFct _ -> assert false
	| Mg -> 1,1,1
	| Next _ -> 1,1,0
	| ResetDer -> assert false
	| RecordPat | InnerRecord _ -> assert false
	| Record | InnerRecordPat _ -> assert false
	| Init s -> (if s <> "" then 1 else 0),1,0
	| Disc -> 1,1,0
	| Test -> 1,1,0
	| InvState -> 0,0,0
	| App n -> n,1,1
	| PartApp _ -> 1,1,0
	| Mod -> 1,1,1
	| Forall -> assert false

(** [topOutputs node_type] takes a node_type and returns the number of outputs to place on the north side *)
let topOutputs node_type = match node_type with
	| Deconstr (_,n) -> (n-1)
	| _ -> 0


(** [topInputs node_type] takes a node_type and returns the number of inputs to place on the north side *)
let topInputs node_type = match node_type with
	| Constr (_,n) -> n
	| VertText _ -> 1
	| PartApp (_,_,n) -> n
	| _ -> 0


(** [botInputs node_type] takes a node_type and returns the number of inputs to place on the south side *)
let botInputs node_type = match node_type with
	| _ -> 0


(** [botOutputs node_type] takes a node_type and returns the number of outputs to place on the south side *)
let botOutputs node_type = match node_type with
	| VertText _ -> 1
	| _ -> 0

(** [is_output_not nt] return true if the node type [nt] is [Nand] or [Not] *)
let is_output_not node_type = match node_type with
	| Nand | Not -> true
	| _ -> false

(** [is_output_question nt] return true if the node type [nt] is [Scond] *)
let is_output_question node_type = match node_type with
	| Scond _ -> true
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
		addOuterNames node#getInputs inputs
		(*let offsets = List.init n (fun i ->
			let i = i + 1 in
			let delta = if i >= 10 then
				8.0 -. 0.013 *. (float_of_int n)
			else 8.0 -. 0.045 *. (float_of_int n)
			in
			let ofs = if i >= 10 then (-5.0) -. 0.013 *. (float_of_int n) else 4.0 in
			(-.ofs -. delta *. (float_of_int (i - 1)))
		) in
		addOffsets node#getControl offsets*)
	| Ram -> 
		addOuterNames node#getInputs ["read_addr";"write?";"write_addr";"write_data"]
	| _ -> ()

(** [needOffsets node node_type] adds offset to some ports of the [node] depending on the [node_type] *)
let needOffsets node node_type = 
	match node_type with
	| Match l ->
		addOffsets node#getControl [-.3.0 -. 3.0 *. (float_of_int (List.length l))]
	| Mux -> 
		addOffsets node#getControl [-3.5]
	| Cond n ->
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
	| Scond _ ->
		let offsets = List.init (List.length node#getInputs) (fun i ->
			-.3.0) in
		addOffsets node#getInputs offsets;
		addOffsets node#getOutputs [-.5.0]
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


(** [new_edge edge_type source target] creates an iEdge of type [type_edge] from the endpoint [source] to the endpoint [target] 

Option [lab] allows to add a label to the created edge, default [None]. *)
let new_edge ?(lab=None) ?(prio=0) edge_type (source : iEndPoint) target = 
	let edge = new iEdge in
	edge#setType edge_type;
	edge#setTarget target#getNode;
	edge#setTargetPort target#getPort;
	edge#setSource source#getNode;
	edge#setSourcePort source#getPort;
	edge#setDirectionPrio prio;
	begin match lab with
	| None -> 
		()
	| Some lab -> edge#addLabel lab;
	end;
	List.iter (fun lab ->
		lab#setPosition Tail;
		edge#addLabel lab
	) source#eatLabels;
	List.iter (fun lab ->
		lab#setPosition Head;
		edge#addLabel lab) target#eatLabels;
	(source#getPort)#addEdge edge;
	(target#getPort)#addBackEdge edge

(** [automaton_edge_type half first reset beginning] gives the type of the automaton edge depending on the reset, beginning, half and first.*)
let automaton_edge_type half first reset beginning = 
	match half, first, reset , beginning with
	| true, true, _ , true -> Aut_first_half_begin
	| true, true, _ , _ -> Aut_first_half
	| true,false,true,true -> Aut_second_half_begin
	| true,false,true,false -> Aut_second_half_end
	| true,false,false,_ -> Aut_second_half_history
	| _,_,true , true -> Aut_begin
	| _,_,true, false -> Aut_end
	| _,_,false , true -> Aut_begin_history
	| _,_,false, false -> Aut_end_history

(** [automaton_edge source target lab r b] creates an automaton edge between the [source] node and [target] node, with label [lab].
[r] and [b] are used to determine which type of automaton edge it is, as the options [half] and [first] are used for.

Option [inline] to set the label to inlined or not. *)
let automaton_edge ?(inline=false) ?(half=false) ?(first=false) source target lab reset beginning = 
	let edge_type = automaton_edge_type half first reset beginning in
	let edge = new iEdge in
	edge#setType edge_type;
	edge#setTarget target;
	edge#setSource source;
	let label = new iEdgeLabel lab in
	label#setPosition Center;
	label#setInlined inline;
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


let mk_portType ?(look=Invisible) ?(side=Undefined) () = {look = look ; side = side}

(** [simpleOpNode node_type parent layer] creates an iNode corresponding to the [node_type], 
with number of ports, names and innerLinks depending on the [node_type], and parent [parent]. 
The [layer] is used for coloring.

Order of the ports for inputs and outputs :  
	South; North; Default (West for inputs, East for outputs)
*)
let simpleOpNode node_type parent layer =
	let nb_inputs,nb_outputs, nb_control = number_ports node_type in
	let node = new iNode in
	node#setType node_type;
	node#setLayer layer;
	
	node#addInputList (create_n_ports (botInputs node_type) node {look = Invisible; side = South});
	node#addOutputList (create_n_ports (botOutputs node_type) node {look = Invisible; side = South});
	node#addOutputList (create_n_ports (topOutputs node_type) node {look = Invisible; side = North});
	node#addInputList (create_n_ports (topInputs node_type) node {look = Invisible; side = North});
	node#addInputList (create_n_ports nb_inputs node {look = Invisible; side = Input});
	let outputLook = if (is_output_not node_type) then Not else Invisible in
	node#addOutputList (create_n_ports nb_outputs node {look = outputLook; side=Output});
	node#addControlList (create_n_ports nb_control node {look = Visible; side=Control});
	addNames node node_type;
	needOffsets node node_type;
	linkCreation node;
	
	parent#addChild node;
	node

(** [simpleRecordNode r_type i_type name_list parent layer] creates an iNode of the type [r_type] similarly to [simpleOpNode],
but there are nodes inside the operation node to create, so it is separated *)
let simpleRecordNode record_type inner_type name_list parent layer = 
	let node = new iNode in
	node#setType record_type;
	node#setLayer layer;
	if record_type = RecordPat then node#addOutputList (create_n_ports (List.length name_list) node {look=Invisible;side=North})
	else node#addInputList (create_n_ports (List.length name_list) node {look=Invisible;side=North});
	if record_type = RecordPat then node#addInputList (create_n_ports 1 node {look=Invisible;side=West})
	else node#addOutputList (create_n_ports 1 node {look=Invisible;side=East});

	parent#addChild node;
	List.iter2 (fun name outer ->
		let inner_node = new iNode in
		inner_node#setType (inner_type name);
		inner_node#setLayer layer;
		let outSide = match record_type with
		| RecordPat -> North
		| Record -> East
		| _ -> assert false
		in
		let portType = {look=Buble;side=outSide} in
		inner_node#addOutputList (create_n_ports 1 inner_node portType);
		node#addChild inner_node;
		let source = outerToEndPoint (List.hd inner_node#getOutputs) in
		let target = outerToEndPoint outer in
		new_edge Simple source target; 
	) name_list (if record_type = RecordPat then (node#getOutputs) else node#getInputs);
	node

type fctParent = 
	| Node of iNode
	| Graph of iGraph


type additionalPort = {
	side : portSide;
	name : string;
}

let mk_addPort ?(name="") side = 
	{side=side; name = name}
(** [simpleFunctionNode node_type input_names output_names parent layer] creates an iNode for a [node_type] function, with 
input ports having names [input_names], and output ports having names [output_names], and parent [parent]. 

Option [control] if there should be a control port on the node 

Option [order] if the node has fixed port order.

Option [vis] if the ports are visible (default [true]).

Option [addI] and [addO] if an additional port has to be added for the inputs and outputs, then the type is given.
*)
let simpleFunctionNode (*?(outBot=false) ?(outTop=false)*) ?(order=false) ?(vis=true) ?(control=false) ?(addI=[]) ?(addO=[]) node_type input_names output_names parent layer = 
	let node = new iNode in
	node#setType node_type;
	node#setLayer layer;
	node#setForceOrder order;

	let rec addPortsInputs addList =
		let look = if vis then Visible else Invisible in
		match addList with
		| [] -> []
		| x :: q -> node#addInputList (create_n_ports 1 node {look=look;side=x.side}); x.name :: (addPortsInputs q)
	in
	let true_input_names = (addPortsInputs addI) @ input_names in	
(*
	match addI with
	| None -> input_names
	| Some pt -> 
		node#addInputList (create_n_ports ~vis:vis 1 node pt);
		"" :: input_names
	in*)

	let rec addPortsOutputs addList =
		let look = if (is_output_question node_type) then Question else if vis then Visible else Invisible in
		match addList with
		| [] -> []
		| x :: q -> node#addOutputList (create_n_ports 1 node {look=look;side=x.side}); x.name :: (addPortsOutputs q)
	in

	let true_output_names = 
		(addPortsOutputs addO) @ output_names in
	(*match addO with
	| None -> output_names
	| Some pt ->
		node#addOutputList (create_n_ports ~vis:vis ~question:(is_output_question node_type) 1 node pt);
		"" :: output_names
	in*)
	let portsLook = if vis then Visible else Invisible in
	node#addInputList (create_n_ports (List.length input_names) node {look=portsLook;side=Input});
	let portsLook = if is_output_question node_type then Question else portsLook in
	node#addOutputList (create_n_ports (List.length output_names) node {look=portsLook;side=Output});
	if control then node#addControlList (create_n_ports 1 node {look=Visible;side=North}); 
	addOuterNames node#getInputs true_input_names;
	addOuterNames node#getOutputs true_output_names;
	needOffsets node node_type;
	begin match parent with
	| Node p -> p#addChild node;
	| Graph g -> g#addNode node;
	end;
	node

(** [addReset node] adds a port of type control to the [node] *)
let addReset node = 
	node#addControlList (create_n_ports 1 node {look=Visible;side=Control})

