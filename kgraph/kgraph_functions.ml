open Kgraph_objects
open Label

let setParent (parent : knode) (child : knode) = 
	if parent.kgraph_id <> child.kgraph_id then
		failwith "not in same kgraph";
	let rec find_loop node wrong_id = 
		match node.parent with
		| Knode parent -> parent.id <> wrong_id && find_loop parent wrong_id
		| Kgraph _ -> false
	in
	if find_loop parent child.id then
		failwith "loop creation of nodes";
	begin match child.parent with
	| Knode _ -> ()
	| Kgraph kgraph ->
		kgraph.node_list <- List.filter (fun (n : knode) -> n.id <> child.id) kgraph.node_list
	end;
	child.parent <- Knode parent;
	parent.children <- child :: parent.children


let setSource (edge : kedge) (source : knode) = 
	if edge.kgraph_id <> source.kgraph_id then
		failwith "not in same kgraph";
	begin match edge.source with
	| Kgraph _ -> ()
	| Knode node ->
		node.outgoingEdges <- List.filter (fun (n : kedge) -> n.id <> edge.id) node.outgoingEdges
	end;
	edge.source <- Knode source;
	source.outgoingEdges <- edge :: source.outgoingEdges

let setTarget (edge : kedge) (target : knode) =
	if edge.kgraph_id <> target.kgraph_id then
		failwith "not in same kgraph";
	begin match edge.target with
	| Kgraph _ -> ()
	| Knode node ->
		node.incomingEdges <- List.filter (fun (n : kedge) -> n.id <> edge.id) node.incomingEdges
	end;
	edge.target <- Knode target;
	target.incomingEdges <- edge :: target.incomingEdges

let getSource (edge : kedge) = 
	match edge.source with
	| Kgraph _ -> failwith "no source defined"
	| Knode node -> node

let getTarget (edge : kedge) = 
	match edge.target with
	| Kgraph _ -> failwith "no target defined"
	| Knode node -> node

let setPort (node : knode) (port : kport) = 
	if node.kgraph_id <> port.kgraph_id then
		failwith "not in same kgraph";
	begin match port.node with
	| Kgraph _ -> ()
	| Knode n ->
		n.ports <- List.filter (fun (p : kport) -> p.id <> port.id) n.ports
	end;
	port.node <- Knode node;
	node.ports <- port::node.ports

let getNodePort port =
	match port.node with
	| Kgraph _ -> failwith "not found"
	| Knode n -> n
let setSourcePort (edge : kedge) (port : kport) = 
	begin match edge.source with
	| Kgraph _ -> failwith "source node not defined"
	| Knode node ->
		begin match port.node with
		| Kgraph _ -> failwith "port node not defined"
		| Knode n ->
		if node.id <> n.id then failwith "source node an port are not the same"
		end
	end;
	edge.sourcePort <- Kport port

let setTargetPort (edge : kedge) (port : kport) = 
	begin match edge.target with
	| Kgraph _ -> failwith "target node not defined"
	| Knode node ->
		begin match port.node with
		| Kgraph _ -> failwith "port node not defined"
		| Knode n ->
		if node.id <> n.id then failwith "target node an port are not the same"
		end
	end;
	edge.targetPort <- Kport port

let addLabeltoNode (node : knode) label =	
	node.labels <- label :: node.labels

let addLabeltoEdge (edge : kedge) label = 
	edge.labels <- label :: edge.labels

let addLabeltoPort (port : kport) label = 
	port.labels <- label :: port.labels

let setNodeContainer (node : knode) cont =
	node.container <- cont

let setEdgeContainer (edge : kedge) cont = 
	edge.container <- cont

let setPortContainer (port : kport) cont =
	port.container <- Some cont

let addNodeData (node : knode) data =
	node.data <- data :: node.data

let addEdgeData (edge : kedge) data =
	edge.data <- data :: edge.data

let addPortData (port : kport) data =
	port.data <- data :: port.data
