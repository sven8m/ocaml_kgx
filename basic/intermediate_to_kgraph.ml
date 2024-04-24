open Intermediate_graph
open Graph_utils

let portTbl = Hashtbl.create 42
let nodeTbl = Hashtbl.create 42


let go_deeper layer = 
	(!Cli_options.do_rec_viz = (-1) || layer = 0 || (layer <= !Cli_options.do_rec_viz))

let rec translate_edge ?(sourcePort) kg sourceNode edge = 
	Format.printf "%d@." edge#getTarget#getLayer;
	if go_deeper (edge#getTarget#getLayer - 1) then begin
		Format.printf "findin...@.";
		let targetNode = Hashtbl.find nodeTbl edge#getTarget#getId in
		Format.printf "found node...@.";
		let targetPort = match edge#getTargetPort with
		| None -> None
		| Some p -> Some (Hashtbl.find portTbl p#getId)
		in
		Format.printf "found@.";
		match edge#getType with
		| Simple | Mult ->
			let sourcePort = match sourcePort with
			| None -> assert false
			| Some p -> p
			in
			let targetPort = match targetPort with
			| None -> assert false
			| Some p -> p
			in
			new_edge ~mult:(edge#getType=Mult) kg sourceNode sourcePort targetNode targetPort edge#getLabels
		| Aut_begin | Aut_end | Aut_begin_history | Aut_end_history ->
			automaton_edge kg edge#getType sourceNode targetNode edge#getLabels;
		| Seq | Seq_half ->
			seq_edge ~half:(edge#getType=Seq_half) ~sourcePort:sourcePort ~targetPort:targetPort kg sourceNode targetNode edge#getLabels 
	end

and translate_port kg (port : iPort) = 
	if Hashtbl.mem portTbl port#getId then
		Hashtbl.find portTbl port#getId
	else begin
		let kn = Hashtbl.find nodeTbl port#getParent#getId in	
		let kp = match port#isNot, port#getType, port#isVisible with
		| true, _ , _ -> notOutputPort kg kn
		| _, Input, true -> visibleInputPort kg kn
		| _, Input, false -> invisibleInputPort kg kn
		| _, Output, true -> visibleOutputPort kg kn
		| _, Output, false -> invisibleOutputPort kg kn
		| _, Control, _ -> visibleControlPort ~ofs:(port#getOffset) kg kn
		| _, Undefined,true -> visiblePort kg kn
		| _, Undefined,false -> invisiblePort kg kn in
		Hashtbl.replace portTbl port#getId kp;
		if port#getName <> "" then begin
			Format.printf "%s@." port#getName;
			let lab = new Label.label in
			lab#setText port#getName;
			kp#addLabel lab;
		end;
		kp
	end

and translate_port_edges kg port =
	let kn = Hashtbl.find nodeTbl port#getParent#getId in
	let kp = Hashtbl.find portTbl port#getId in
	Format.printf "high : %d@." port#getParent#getLayer;
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
			simpleAndNode kg
		| Or _ -> 
			simpleOrNode kg
		| Xor _ -> 
			simpleXorNode kg
		| Nand -> 
			simpleNandNode kg
		| Mux ->
			simpleMuxNode kg
		| Reg ->
			simpleRegNode kg
		| Buffer ->
			simpleBufferNode kg
		| Not ->
			simpleNotNode kg
		| Fby ->
			simpleFbyNode kg
		| Cond _->
			simpleCondNode kg
		| Every s ->
			function_node kg s node#getLayer 	
		| Fct s ->
			function_node kg s node#getLayer
		| Slice (i,j) ->
			simpleSliceNode kg i j
		| Select i ->
			simpleSelectNode kg i
		| Concat ->
			simpleConcatNode kg
		| Match _ ->
			simpleMatchNode kg
		| Match_node ->
			functionReset ~m:true kg "match" node#getLayer
		| Match_state s ->
			function_node kg s node#getLayer
		| Reset ->
			functionReset ~res:true kg "reset" node#getLayer
		| Aut ->
			function_node ~aut:true kg "automaton" node#getLayer	
		| Aut_state (s,init) ->
			stateNode ~init:init kg s node#getLayer
		| For ->
			simpleSeqBlockNode kg "for"
		| While ->
			simpleSeqBlockNode kg "while"
		| Pause init ->
			simpleBubleNode ~init:init kg 
		| Ram ->
			ramNode kg
		| Rom ->
			romNode kg
		| Const (s,var) ->
			simpleConstNode ~const:(not var) kg s
		| Tuple _ -> simpleTupleNode kg
		| UnTuple _ -> simpleUnTupleNode kg
		| Sink (s,used) ->
			simpleSinkNode ~used:used kg s
		| Var s ->
			simpleInputVarNode kg s	
		| Sync init ->
			simpleSyncNode ~init:init kg
		| Final ->
			terminalSyncNode kg		
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

and translate_node_edges kg node = 
	let kn = Hashtbl.find nodeTbl node#getId in
	List.iter (fun edge -> translate_edge kg kn edge) node#getEdges;
	List.iter (fun port ->
		translate_port_edges kg port) node#getPorts;
	if (go_deeper node#getLayer) then
		List.iter (fun child ->
			translate_node_edges kg child) node#getChildren
	
let translate_graph ig = 
	let kg = init_kgraph () in
	List.iter (fun node -> ignore (translate_node kg node)) ig#getNodes;
	Format.printf "creation done@.";
	List.iter (fun node -> translate_node_edges kg node) ig#getNodes;
	kg
