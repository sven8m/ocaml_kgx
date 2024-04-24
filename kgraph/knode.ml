open ObjectSig

class knode graph : knodeSig = object (self) 
	inherit Kelement.kelement graph
	inherit Object_pos.obj_pos
	val mutable parent = None
	val mutable children = []
	val mutable incomingEdges = []
	val mutable outgoingEdges = [] 
	val mutable ports = []
	val mutable path = ""
	
	initializer kgraph#addNode (self :> knode)
	method delChild node = 
		children <- List.filter (fun (n : knode) -> n#getId <> node#getId ) children
	method addChild node = 
		children <- node :: children
	method getChildren = List.rev children

	method getParent = parent

	method setParent (par : knode) = 
		if par#getGraphId <> kgraph_id then
			failwith "not in same kgraph";
		let rec find_loop node wrong_id = 
			match node#getParent with
			| Some p -> p#getId <> wrong_id && find_loop p wrong_id
			| None -> false
		in
		if find_loop par id then
			failwith "loop creation of nodes";
		begin match parent with
		| Some _ -> ()
		| None ->
			kgraph#delNode (self :> knode);
		end;
		parent <- Some par;
		par#addChild (self :> knode)

	method getIncomingEdges = List.rev incomingEdges
	method addIncomingEdge edge = 
		incomingEdges <- edge :: incomingEdges
	method delIncomingEdge edge = 
		incomingEdges <- List.filter (fun e -> e#getId <> edge#getId) incomingEdges

	method getOutgoingEdges = List.rev outgoingEdges
	method addOutgoingEdge edge = 
		outgoingEdges <- edge :: outgoingEdges 
	method delOutgoingEdge edge = 
		outgoingEdges <- List.filter (fun e -> e#getId <> edge#getId) outgoingEdges

	method getPorts = List.rev ports
	method addPort port = 
		ports <- port :: ports
	method delPort port = 
		ports <- List.filter (fun p -> p#getId <> port#getId) ports
	method getPath = path
	method setPath s = path <- s 

end

