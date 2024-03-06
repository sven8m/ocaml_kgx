type parent = 
	Knode of knode | Kgraph of kgraph

and knode = {
	mutable id : int;
	kgraph_id : int;
	mutable parent : parent;
	mutable children : knode list;
	mutable incomingEdges : kedge list;
	mutable outgoingEdges : kedge list;
	mutable path : string
}

and endpoint = 
	Knode of knode | Kgraph of kgraph

and kedge = {
	mutable id : int;
	kgraph_id : int;
	mutable source : endpoint;
	mutable target : endpoint
}

and kgraph = {
	id : int;
	mutable cnt : int;
	mutable node_list : knode list;
}


let id_kgraph = ref 0 

let create_kgraph () = 
	let kgraph = {id = !id_kgraph; cnt = 0 ; node_list = []} in
	incr id_kgraph;
	kgraph

let create_knode kgraph = 
	let node = 
		{id = kgraph.cnt ; 
		kgraph_id = kgraph.id ; 
		parent = Kgraph kgraph ; 
		children = [] ; 
		incomingEdges = [] ; 
		outgoingEdges = [] ;
		path = ""} in
	kgraph.cnt <- kgraph.cnt + 1;
	kgraph.node_list <- node :: kgraph.node_list;
	node

let create_kedge kgraph = 
	let kedge = {id = kgraph.cnt ; kgraph_id = kgraph.id ; source = Kgraph kgraph ; target = Kgraph kgraph} in
	kgraph.cnt <- kgraph.cnt + 1;
	kedge
