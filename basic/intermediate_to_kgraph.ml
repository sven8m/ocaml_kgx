open Intermediate_graph
open Graph_utils

let portTbl = Hashtbl.create 42
let nodeTbl = Hashtbl.create 42


let go_deeper layer = 
	(!InterLib_options.do_rec_viz = (-1) || layer = 0 || (layer <= !InterLib_options.do_rec_viz))

let passOpt el = 
	match el with
	| None -> assert false
	| Some el -> el

let rec translate_edge ?(sourcePort) kg sourceNode edge = 
	(*Format.printf "%d@." edge#getTarget#getLayer;*)
	if go_deeper (edge#getTarget#getLayer - 1) then begin
		(*Format.printf "findin...@.";*)
		let targetNode = Hashtbl.find nodeTbl edge#getTarget#getId in
		(*Format.printf "found node...@.";*)
		let targetPort = match edge#getTargetPort with
		| None -> None
		| Some p -> Some (Hashtbl.find portTbl p#getId)
		in
		(*Format.printf "found@.";*)
		match edge#getType with
		| Simple | Mult ->
			let sourcePort = passOpt sourcePort in
			let targetPort = passOpt targetPort in
			new_edge ~custom:(edge:>iInformation) ~mult:(edge#getType=Mult) kg sourceNode sourcePort targetNode targetPort edge#getLabels
		| Aut_begin | Aut_end | Aut_begin_history | Aut_end_history ->
			automaton_edge kg edge#getType sourceNode targetNode edge#getLabels;
		| Seq | Seq_half ->
			seq_edge ~half:(edge#getType=Seq_half) ~sourcePort:sourcePort ~targetPort:targetPort kg sourceNode targetNode edge#getLabels 
		| DepLink -> 
			if !InterLib_options.do_show_link || !InterLib_options.do_show_all then begin
				let sourcePort = passOpt sourcePort in
				let targetPort = passOpt targetPort in
				linkEdge ~custom:(edge:>iInformation) kg sourceNode sourcePort targetNode targetPort;
			end
		| DepAutLink | Link | AutLink -> 
			if !InterLib_options.do_show_all then begin
				let sourcePort = passOpt sourcePort in
				let targetPort = passOpt targetPort in
				linkEdge ~custom:(edge:>iInformation) kg sourceNode sourcePort targetNode targetPort
			end
	end

and translate_port kg (port : iPort) = 
	if Hashtbl.mem portTbl port#getId then
		Hashtbl.find portTbl port#getId
	else begin
		let kn = Hashtbl.find nodeTbl port#getParent#getId in	
		let kp = match port#isNot, port#getType, port#isVisible with
		| true, _ , _ -> notOutputPort ~custom:(port:>iInformation) kg kn
		| _, Input, true -> visibleInputPort ~custom:(port:>iInformation) kg kn
		| _, Input, false -> invisibleInputPort ~custom:(port:>iInformation) kg kn
		| _, Output, true -> visibleOutputPort ~custom:(port:>iInformation) kg kn
		| _, Output, false -> invisibleOutputPort ~custom:(port:>iInformation) kg kn
		| _, Control, _ -> visibleControlPort ~custom:(port:>iInformation) ~ofs:(port#getOffset) kg kn
		| _, Undefined,true -> visiblePort ~custom:(port:>iInformation) kg kn
		| _, Undefined,false -> invisiblePort ~custom:(port:>iInformation) kg kn in
		Hashtbl.replace portTbl port#getId kp;
		if port#getName <> "" then begin
			Format.printf "%s@." port#getName;
			let lab = portLabel ~custom:(port:>iInformation) port#getName in
			kp#addLabel lab;
		end;
		kp
	end

and translate_port_edges kg port =
	let kn = Hashtbl.find nodeTbl port#getParent#getId in
	let kp = Hashtbl.find portTbl port#getId in
	(*Format.printf "high : %d@." port#getParent#getLayer; *)
	List.iter (fun edge -> translate_edge kg ~sourcePort:kp kn edge) port#getEdges

(** [revInputports node] reverse the order for the ports of type [Input] *)
and revInputPorts node = 	
	let input_ports , other_ports = List.partition (fun port ->
		port#getType = Input) node#getPorts in
	node#setPorts (other_ports @ (List.rev input_ports))

and translate_node kg node =
	if Hashtbl.mem nodeTbl node#getId then
		Hashtbl.find nodeTbl node#getId
	else begin
		revInputPorts node;
		let kn = match node#getType with
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
			function_node ~custom:(node:>iInformation) kg s node#getLayer 	
		| Fct s ->
			function_node ~custom:(node:>iInformation) kg s node#getLayer
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
		in
		Hashtbl.replace nodeTbl node#getId kn;
		List.iter (fun port ->
			ignore (translate_port kg port)) node#getPorts;
		if (go_deeper node#getLayer) then
			List.iter (fun child ->
				let kc = translate_node kg child in
				kc#setParent kn) node#getChildren; 
		kn
	end

and translate_node_edges kg (node : iNode) = 
	let kn = Hashtbl.find nodeTbl node#getId in
	List.iter (fun edge -> translate_edge kg kn edge) node#getEdges;
	List.iter (fun port ->
		translate_port_edges kg port) node#getPorts;
	if (go_deeper node#getLayer) then
		List.iter (fun child ->
			translate_node_edges kg child) node#getChildren
	
let translate_graph (ig : iGraph) = 
	let kg = init_kgraph () in
	List.iter (fun node -> ignore (translate_node kg node)) ig#getNodes;
	Format.printf "creation done@.";
	List.iter (fun node -> translate_node_edges kg node) ig#getNodes;
	kg
