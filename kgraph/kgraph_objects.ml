open Styles
open Container
open Label
open Rendering

type parent = 
	Knode of knode | Kgraph of kgraph

and knode = {
	mutable id : int;
	kgraph_id : int;
	mutable parent : parent;
	mutable children : knode list;
	mutable incomingEdges : kedge list;
	mutable outgoingEdges : kedge list;
	mutable ports : kport list;
	mutable path : string;
	mutable labels : label list;
	mutable container : container;
	mutable data : rendering list
}

and endpoint = 
	Knode of knode | Kgraph of kgraph

and port_endpoint = 
	Kport of kport | Kgraph of kgraph

and kedge = {
	mutable id : int;
	kgraph_id : int;
	mutable source : endpoint;
	mutable target : endpoint;
	mutable sourcePort : port_endpoint;
	mutable targetPort : port_endpoint;
	mutable labels : label list;
	mutable container : container;
	mutable data : rendering list
}

and kgraph = {
	id : int;
	mutable cnt : int;
	mutable node_list : knode list;
}

and kport = {
	mutable id : int;
	kgraph_id : int;
	mutable node : parent;
	mutable labels : label list;
	mutable container : container option;
	mutable data : rendering list
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
		path = "";
		labels = [];
		container = Rect;
		data = [];
		ports = []} in
	kgraph.cnt <- kgraph.cnt + 1;
	kgraph.node_list <- node :: kgraph.node_list;
	node

let create_kedge kgraph = 
	let kedge = 
		{id = kgraph.cnt ; 
		kgraph_id = kgraph.id ; 
		source = Kgraph kgraph ; 
		target = Kgraph kgraph ;
		sourcePort = Kgraph kgraph;
		targetPort = Kgraph kgraph;
		labels = [] ; 
		container = PolyLine ;
		data = [] } in
	kgraph.cnt <- kgraph.cnt + 1;
	kedge

let create_kport kgraph = 
	let kport =
		{id = kgraph.cnt;
		kgraph_id = kgraph.id;
		node = Kgraph kgraph;
		labels = [];
		container = None;
		data = [] } in
	kgraph.cnt <- kgraph.cnt + 1;
	kport

