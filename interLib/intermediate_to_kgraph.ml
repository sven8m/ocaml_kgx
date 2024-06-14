open Intermediate_graph
open Graph_utils

let portTbl = Hashtbl.create 42
let nodeTbl = Hashtbl.create 42


(** [go_deeper layer] checks if the layer has to be shown *)
let go_deeper layer = 
	(!InterLib_options.do_rec_viz = (-1) || layer = 0 || (layer <= !InterLib_options.do_rec_viz))

let edge_between_text edge = 
	let source = edge#getSource in
	let target = edge#getTarget in
	if source#getAsText && source#getLayer < target#getLayer then
		(*edge to inside of text -> no *) true
	else false

let passOpt el = 
	match el with
	| None -> assert false
	| Some el -> el

let print_edge_type edge = 
	let text = match edge#getType with
	| Link -> "Link"
	| Simple -> "Simple"
	| AutLink -> "AutLink"
	| Aut_begin -> "Autbegin"
	| Aut_begin_history -> "Aut_begin_history"
	| Aut_end -> "Aut_end"
	| Aut_end_history -> "Aut_end_history"
	| DepAutLink -> "DepAutLink"
	| DepLink -> "DepLink"
	| Mult -> "Mult"
	| Seq -> "Seq"
	| Seq_half -> "Seq_half"
	| Big -> "Big"
	| Aut_first_half -> "Aut_first_half"
	| Aut_first_half_begin -> "Aut_first_half_begin"
	| Aut_second_half_begin -> "Aut_second_half_begin"
	| Aut_second_half_end -> "Aut_second_half_end"
	| Aut_second_half_history -> "Aut_second_half_history"
	in
	Format.printf "%s@." text


(** [translate_edge kg sourceNode edge] takes an iEdge [edge] and translates it to a [KEdge], given the KGraph [kg], the source KNode [sourceNode]
and the potential source KPort [sourcePort] *)
let rec translate_edge ?(sourcePort) (kg : Kgraph.kgraph) sourceNode (edge : iEdge) = 
(*	Format.printf "will translate edge@.";
*)	if go_deeper (edge#getTarget#getLayer - 1) && ((edge#getTarget)#getType <> Link || !InterLib_options.do_show_all) && ((edge#getSource)#getType <> Link || !InterLib_options.do_show_all) && (not (edge_between_text edge)) then begin
		(*Format.printf "entering @.";
		Format.printf "%b %b@." edge#getSource#getAsText edge#getTarget#getAsText;
		print_edge_type edge;			*)
		let targetNode = Hashtbl.find nodeTbl edge#getTarget#getId in
		let targetPort = match edge#getTargetPort with
		| None -> None
		| Some p -> Some (Hashtbl.find portTbl p#getId)
		in
		(*Format.printf "found target@."; *)
		let kedge = match edge#getType with
		| Simple | Mult | Big ->
			let sourcePort = passOpt sourcePort in
			let targetPort = passOpt targetPort in
			let translateThickness e_t = 
				match e_t with
				| Simple -> Gu_Simple
				| Mult -> Gu_Mult
				| Big -> Gu_Big
				| _ -> assert false
			in
			let kedge = new_edge ~custom:(edge:>iInformation) ~thick:(translateThickness edge#getType) kg sourceNode sourcePort targetNode targetPort in
			Some kedge
		| Aut_begin | Aut_end | Aut_begin_history | Aut_end_history | Aut_first_half | Aut_first_half_begin | Aut_second_half_begin | Aut_second_half_end | Aut_second_half_history ->
			Some (automaton_edge ~custom:(edge:>iInformation) kg edge#getType sourceNode targetNode)
		| Seq | Seq_half ->
			Some (seq_edge ~half:(edge#getType=Seq_half) ~sourcePort:sourcePort ~targetPort:targetPort kg sourceNode targetNode)
		| DepLink -> 
			if !InterLib_options.do_show_link || !InterLib_options.do_show_all then begin
				let sourcePort = passOpt sourcePort in
				let targetPort = passOpt targetPort in
				linkEdge ~custom:(edge:>iInformation) kg sourceNode sourcePort targetNode targetPort;
			end;
			None
		| DepAutLink | Link | AutLink -> 
			if !InterLib_options.do_show_all then begin
				let sourcePort = passOpt sourcePort in
				let targetPort = passOpt targetPort in
				linkEdge ~custom:(edge:>iInformation) kg sourceNode sourcePort targetNode targetPort
			end;
			None
		in
		begin match kedge with
		| None -> ()
		| Some kedge ->
			List.iter (fun label ->
				let klab = labelOfEdgeLabel ~custom:(edge:>iInformation) label#getName label#getPosition in
				kedge#addLabel klab;
			) edge#getLabels
		end
	end
	(*Format.printf "has finished edge@."
*)
(** [translate_port kg port] takes an iPort [port] and translates it to a KPort given the Kgraph [kg] *)
and translate_port (kg : Kgraph.kgraph) (port : iPort) = 
	if Hashtbl.mem portTbl port#getId then
		Hashtbl.find portTbl port#getId
	else begin
		let kn = Hashtbl.find nodeTbl port#getParent#getId in
		let kp =
		if port#isBuble then
			match port#getType with
			| OutputTop ->
				bublePort ~custom:(port:>iInformation) ~ofs:(port#getOffset) kg kn
			| Output ->
				bublePort ~dir:East ~custom:(port:>iInformation) ~ofs:(port#getOffset) kg kn
			| _ -> assert false
		else if port#isNot then
			notOutputPort ~custom:(port:>iInformation) ~ofs:(port#getOffset) kg kn
		else if port#isQuestion then
			questionOutputPort ~custom:(port:>iInformation) ~ofs:(port#getOffset) kg kn
		else 
		match port#getType, port#isVisible with
		| Input, true -> visibleInputPort ~custom:(port:>iInformation) ~ofs:(port#getOffset) kg kn
		| Input, false -> invisibleInputPort ~custom:(port:>iInformation) ~ofs:(port#getOffset) kg kn
		| Output, true -> visibleOutputPort ~custom:(port:>iInformation) ~ofs:(port#getOffset) kg kn
		| Output, false -> invisibleOutputPort ~custom:(port:>iInformation) ~ofs:(port#getOffset) kg kn
		| Control, _ -> visibleControlPort ~custom:(port:>iInformation) ~ofs:(port#getOffset) kg kn
		| OutputTop , _ -> invisibleControlPort ~custom:(port:>iInformation) ~ofs:(port#getOffset) kg kn
		| InputTop , _ -> invisibleControlPort ~custom:(port:>iInformation) ~ofs:(port#getOffset) kg kn
		| Undefined,true -> visiblePort ~custom:(port:>iInformation) ~ofs:(port#getOffset) kg kn
		| Undefined,false -> invisiblePort ~custom:(port:>iInformation) ~ofs:(port#getOffset) kg kn 
		| (InputBot | OutputBot) , true -> visibleBotPort ~custom:(port:>iInformation) ~ofs:(port#getOffset) kg kn
		| (InputBot | OutputBot) , false -> invisibleBotPort ~custom:(port:>iInformation) ~ofs:(port#getOffset) kg kn
		in
		Hashtbl.replace portTbl port#getId kp;
		if port#getName <> "" then begin
			let lab = portLabel ~custom:(port:>iInformation) port#getName in
			kp#addLabel lab;
		end;
		kp
	end

(** [translate_port_edges kg port] takes an iPort [port] and translates its outgoing edges into KEdges *)
and translate_port_edges kg port =
	let kn = Hashtbl.find nodeTbl port#getParent#getId in
	let kp = Hashtbl.find portTbl port#getId in
	List.iter (fun edge -> translate_edge kg ~sourcePort:kp kn edge) port#getEdges

(** [revInputports node] reverse the order for the ports of type [Input] *)
and revInputPorts (node : iNode) = 	
	let input_ports , other_ports = List.partition (fun port ->
		port#getType = Input) node#getPorts in
	node#setPorts (other_ports @ (List.rev input_ports))

(** [getKnodeFromType kgraph node] returns the knode corresponding to the [node_type] of [node]*)
and getKnodeFromType kg node = 
	match node#getType with
	| And _ ->
		simpleAndNode ~custom:(node:>iInformation) kg
	| Or _ -> 
		simpleOrNode ~custom:(node:>iInformation) kg
	| Xor _ -> 
		simpleXorNode ~custom:(node:>iInformation) kg
	| Nand -> 
		simpleNandNode ~custom:(node:>iInformation) kg
	| Mux ->
		simpleMuxNode ~custom:(node:>iInformation) kg
	| Reg ->
		simpleRegNode ~custom:(node:>iInformation) kg
	| Buffer | Not ->
		simpleBufferNode ~custom:(node:>iInformation) kg
	| Fby ->
		simpleFbyNode ~custom:(node:>iInformation) kg
	| Cond _->
		simpleCondNode ~custom:(node:>iInformation) kg
	| Every s ->
		function_node ~order:(node#isForcedOrder) ~custom:(node:>iInformation) kg s node#getLayer 	
	| Fct s ->
		function_node ~order:(node#isForcedOrder) ~custom:(node:>iInformation) kg s node#getLayer
	| Slice (i,j) ->
		simpleSliceNode ~custom:(node:>iInformation) kg i j
	| Select i ->
		simpleSelectNode ~custom:(node:>iInformation) kg i
	| Concat ->
		simpleConcatNode ~custom:(node:>iInformation) kg
	| Match _ ->
		simpleMatchNode ~custom:(node:>iInformation) kg
	| Match_node ->
		function_node ~custom:(node:>iInformation) ~m:true kg "match" node#getLayer
	| Match_state s ->
		function_node ~custom:(node:>iInformation) kg s node#getLayer
	| Reset ->
		function_node ~custom:(node:>iInformation) ~res:true kg "reset" node#getLayer
	| Aut ->
		function_node ~custom:(node:>iInformation) ~aut:true kg "automaton" node#getLayer	
	| Aut_state (s,init) ->
		stateNode ~custom:(node:>iInformation) ~init:init kg s node#getLayer
	| For ->
		simpleSeqBlockNode ~custom:(node:>iInformation) kg "for"
	| While ->
		simpleSeqBlockNode ~custom:(node:>iInformation) kg "while"
	| Pause init ->
		simpleBubleNode ~init:init kg 
	| Ram ->
		ramNode ~custom:(node:>iInformation) kg
	| Rom ->
		romNode ~custom:(node:>iInformation) kg
	| Const (s,var) ->
		simpleConstNode ~custom:(node:>iInformation) ~const:(not var) kg s
	| Tuple _ | UnTuple _ -> simpleTupleNode ~custom:(node:>iInformation) kg
	| Sink (s,used) ->
		simpleSinkNode ~custom:(node:>iInformation) ~used:used kg s
	| Var s ->
		simpleInputVarNode ~custom:(node:>iInformation) kg s	
	| Sync init ->
		simpleSyncNode ~custom:(node:>iInformation) ~init:init kg
	| Final ->
		terminalSyncNode kg		
	| Link -> 
		simpleLinkNode kg
	(* for z *)
	| Add _ ->
		simpleAddNode ~custom:(node:>iInformation) kg
	| Minus _ ->
		simpleMinusNode ~custom:(node:>iInformation) kg
	| Mult _ ->
		simpleTimesNode ~custom:(node:>iInformation) kg
	| Div ->
		simpleDivNode ~custom:(node:>iInformation) kg
	| Last ->
		simpleLastNode ~custom:(node:>iInformation) kg
	| Deconstr (name , n) ->
		simpleDeConstrNode ~custom:(node:>iInformation) kg name n
	| Constr (name, n) ->
		simpleConstrNode ~custom:(node:>iInformation) kg name n
	| Der (name,_) ->
		simpleDerNode ~custom:(node:>iInformation) kg name
	| Inv ->
		simpleInvisibleNode ~custom:(node:>iInformation) kg
	| VertText t ->
		simpleTextNode ~custom:(node:>iInformation) kg t
	| Present ->
		simplePresentNode ~custom:(node:>iInformation) kg
	| Text (s,_) ->
		simpleTextNode ~custom:(node:>iInformation) kg s
	| Period _ ->
		simplePeriodNode ~custom:(node:>iInformation) kg
	| Emit s ->
		simpleEmitNode ~custom:(node:>iInformation) kg s
	| Up ->
		simpleUpNode ~custom:(node:>iInformation) kg
	| Scond (t_opt) ->
		simpleScondNode ~custom:(node:>iInformation) kg t_opt
	| BlanckFct b ->
		simpleBlanckNode ~custom:(node:>iInformation) kg b
	| Mg -> 
		simpleMinusGreaterNode ~custom:(node:>iInformation) kg
	| Next s ->
		simpleNextNode ~custom:(node:>iInformation) kg s
	| ResetDer ->
		simpleResetDerNode ~custom:(node:>iInformation) kg
	| RecordPat | Record ->
		simpleRecordNode ~custom:(node:>iInformation) kg
	| InnerRecord s | InnerRecordPat s ->
		simpleInnerRecordNode ~custom:(node:>iInformation) kg s

(** [translate_node kg node] takes an iNode [node] and translates it into a KNode, its ports into KPorts, and recursively its children. (not the edges) *)
and translate_node kg node =
	if Hashtbl.mem nodeTbl node#getId then
		Hashtbl.find nodeTbl node#getId
	else begin
		revInputPorts node;
		let kn = getKnodeFromType kg node in
		Hashtbl.replace nodeTbl node#getId kn;
		List.iter (fun port ->
			ignore (translate_port kg port)) node#getPorts;
		if (go_deeper node#getLayer) && (not node#getAsText) then
			List.iter (fun child ->
				let kc = translate_node kg child in
				kc#setParent kn) node#getChildren
		else if (node#getAsText) then begin
			let textNode = simpleTextNode ~custom:(node:>iInformation) kg node#getTextContent in
			textNode#setParent kn;
		end;
		kn
	end

(** [translate_node_edges kg node] takes an iNode [node] and translates its edges it into KEdges *)
and translate_node_edges kg (node : iNode) = 
	let kn = Hashtbl.find nodeTbl node#getId in
	List.iter (fun edge -> translate_edge kg kn edge) node#getEdges;
	List.iter (fun port ->
		translate_port_edges kg port) node#getPorts;
	if (go_deeper node#getLayer) && (not node#getAsText) then
		List.iter (fun child ->
			translate_node_edges kg child) node#getChildren


let compare_edges edge1 edge2 = 
	if edge1#getInCycle = edge2#getInCycle then begin
		match edge1#getDead , edge2#getDead with
		| false, false | true,true -> 0
		| false, true -> -1
		| true,false -> 1
	end else begin
		match edge1#getInCycle , edge2#getInCycle with
		| false,false | true,true -> 0
		| false,true -> -1
		| true,false -> 1
	end

let sort_edges (eC : iEdgeContainer) = 
	let edges = eC # getEdges in
	let edges = List.sort compare_edges edges in
	eC #setEdges edges

let rec sort_edges_node node = 
	sort_edges (node:>iEdgeContainer);
	List.iter (fun port ->
		sort_edges (port:>iEdgeContainer)) node#getPorts;
	List.iter sort_edges_node node#getChildren
(** [translate_graph ig] translates the iGraph [ig] into the corresponding Kgraph *)
let translate_graph (ig : iGraph) = 
	let kg = init_kgraph () in
	List.iter sort_edges_node ig#getNodes;
	List.iter (fun node -> ignore (translate_node kg node)) ig#getNodes; (*only the structure *)
	List.iter (fun node -> translate_node_edges kg node) ig#getNodes; (*now add the edges *)
	kg