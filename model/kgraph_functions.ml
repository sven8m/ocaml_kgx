open Kgraph_objects

let setParent (parent : knode) (child : knode) = 
	Format.printf "thwrtjhwr@.";
	if parent.kgraph_id <> child.kgraph_id then
		failwith "not in same kgraph";
	let rec find_loop node wrong_id = 
		match node.parent with
		| Knode parent -> parent.id <> wrong_id && find_loop parent wrong_id
		| Kgraph _ -> false
	in
	if find_loop parent child.id then
		failwith "loop creation of nodes";
	Format.printf "Qtghq@.";	
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
