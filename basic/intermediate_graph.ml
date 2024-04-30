type node_type = 
	| And of int | Or of int | Xor of int | Nand | Mux | Reg | Buffer | Not | Fby | 
	Cond of int | Every of string | Fct of string | Slice of (string * string) | Select of string | Concat | Match of string list | Match_node
	| Match_state of string | Reset | Aut | Aut_state of (string * bool) | For | While | Pause of bool | Ram | Rom | Const of (string * bool)
	| Tuple of int | UnTuple of int | Sink of (string * bool) | Var of string | Sync of bool | Final
type port_type = 
	Input | Output | Control | Undefined

type edge_type = 
	Simple | Mult | Aut_begin | Aut_end | Aut_begin_history | Aut_end_history
	| Seq | Seq_half | DepLink | DepAutLink | Link

type label_placement = Tail | Center | Head | Undef

type edge_label = string * label_placement
let id_cnt = ref 0

class iElement = object
	val id = 
		let i = !id_cnt in
		incr id_cnt;
		i
	val mutable inCycle = false
	method getId = id
	method getInCycle = inCycle
	method setInCycle b = inCycle <- b
end

and iEndPoint n p = object
	val mutable node = (n : iNode)
	val mutable port = (p : iPort)
	val mutable var = false
	val mutable labels = ([] : string list)

	method getNode = node
	method getPort = port
	method isVar = var
	method getLabels = labels

	method setNode n = node <- n
	method setPort p = port <- p
	method setVar b = var <- b
	method addLabel l = labels <- l :: labels
	method eatLabels =
		let l = labels in
		labels <- [];
		l
end

and iEdgeContainer = object
	inherit iElement
	
	val mutable edges = ([] : iEdge list)
	val mutable back_edges = ([] : iEdge list)


	method getEdges = List.rev edges
	method getBackEdges = List.rev back_edges
	method addEdge e = edges <- e :: edges
	method addBackEdge e = back_edges <- e :: back_edges
end

and iNode = object
	inherit iEdgeContainer

	val mutable n_type = Buffer
	val mutable children = ([] : iNode list)
	val mutable inputs = ([] : iOuterPort list)
	val mutable outputs = ([] : iOuterPort list)
	val mutable control = ([] : iOuterPort list)
	val mutable ports = ([] : iPort list)
	val mutable layer = 0

	method getType = n_type
	method getChildren = List.rev children
	method getInputs = List.rev inputs
	method getOutputs = List.rev outputs
	method getControl = List.rev control
	method getPorts = List.rev ports
	method getLayer = layer
	
	method setType t = n_type <- t
	method addChild c = children <- c :: children
	method addInput i = inputs <- i :: inputs
	method addOutput o = outputs <- o :: outputs
	method addControl c = control <- c :: control
	method addInputList l = inputs <- (List.rev l) @ inputs
	method addOutputList l = outputs <- (List.rev l) @ outputs
	method addControlList l = control <- (List.rev l) @ control
	method addPort p = ports <- p :: ports
	method setPorts pl = ports <- List.rev pl
	method setLayer l = layer <- l
end

and iPort node = object
	inherit iEdgeContainer

	val mutable name = ""
	val mutable p_type = (Undefined : port_type)
	val mutable visible = true
	val mutable no = false
	val mutable parent = (node : iNode)
	val mutable ofs = 0.0

	method getName = name
	method getType = p_type
	method isVisible = visible
	method getParent = parent
	method isNot = no
	method getOffset = ofs
	
	method setName n = name <- n
	method setType t = p_type <- t
	method setVisible b = visible <- b
	method setParent p = parent <- p
	method setNot b = no <- b
	method setOffset o = ofs <- o
end

and iOuterPort p = object
	val mutable port = (p : iPort)
	val mutable node = (p#getParent : iNode)
	val mutable inner_ports = ([] : iEndPoint list)

	method getNode = node
	method getPort = port
	method getInnerPorts = 
		match inner_ports with
		| [] -> 
			let endPoint = new iEndPoint node port in
			[endPoint]
		| l -> l
	
	method setPort p = port <- p
	method addInnerPort p = inner_ports <- p :: inner_ports
end

and iEdge = object
	inherit iElement
	val mutable labels = ([] : edge_label list)
	val mutable e_type = Simple
	val mutable target = (None : iNode option)
	val mutable targetPort = (None : iPort option)
	val mutable source = (None : iNode option)
	val mutable sourcePort = (None : iPort option)

	method getLabels = labels
	method getType = e_type
	method getTarget = match target with
	| None -> assert false
	| Some t -> t
	method getTargetPort = targetPort
	method getSource = match source with
	| None -> assert false
	| Some t -> t
	method getSourcePort = sourcePort

	method addLabel l = labels <- l :: labels
	method resetLabel = labels <- []
	method setType t = e_type <- t
	method setTarget t = target <- Some t
	method setTargetPort p = targetPort <- Some p
	method setSource s = source <- Some s
	method setSourcePort p = sourcePort <- Some p
end

and iGraph = object
	val mutable cnt = 0
	val mutable nodes = ([] : iNode list)

	method getCnt = cnt
	method addOne () = 
		let v = cnt in
		cnt <- cnt + 1;
		v
	
	method getNodes = nodes
	method addNode n = nodes <- n :: nodes
end
