(** Defines the types for the intermediate graph. *)


type blanckFctOpt = 
	| NONE
	| SIDE
	| ORDER

type node_type =
	| And of int (**[n] is number of inputs *)
	| Or of int (**[n] is number of inputs *)
	| Xor of int (**[n] is number of inputs *)
	| Nand
	| Mux
	| Reg
	| Buffer
	| Not
	| Fby
	| Cond of int (** [n] is number of conditions *)
	| Every of string (** [s] is the function name *)
	| Fct of string (** [s] is the function name *)
	| Slice of (string * string) (**[i , j] are the slice indexes*)
	| Select of string (**[i] is the select index*)
	| Concat
	| Match of string list (**[sl] are the patterns names*)
	| Match_node
	| Match_state of string (**[s] is the states name *)
	| Reset
	| Aut
	| Aut_state of (string * bool) (**[s,b]. [s] is the states name, [b] if the state is initial*)
	| For
	| While
	| Pause of bool (**[b] is [true] if the state is initial*)
	| Ram
	| Rom
	| Const of (string * bool) (**[s,b]. [s] is the constant, [b] is [true] if the constant is a variable *)
	| Tuple of int (**[n] is the number of inputs *)
	| UnTuple of int (**[n] is the number of outputs *)
	| Sink of (string * bool) (**[s,b]. [s] is the name, [b] is [true] if the variable is used. *)
	| Var of string (**[s] is the name *)
	| Sync of bool (**[b] is [true] if the node is initial.*)
	| Final
	| Link
(* for z *)
	| Add of int
	| Mult of int
	| Minus of int 
	| Div 
	| Last
	| Deconstr of string * int (**[s,n]. [s] is the name of [s x], [n] is the number of arguments *)
	| Constr of string * int (**[s,n]. [s] is name of [s x], [n] is the number of arguments *)
	| Der of string *int (**[s,n]. [s] is the init name, [n] is number of resets *)
	| Inv
	| VertText of string
	| Present 
	| Text of string*int (** [s,n]. [s] is the content, [n] the number of inputs *)
	| Period of bool (** [b]. [b] is if there is a phase *)
	| Emit of string (** [s]. [s] is name of the signal *)
	| Up
	| Scond of string option (** [s,b]. [s] is a title. [b] is if output should be vertical *)
	| BlanckFct of blanckFctOpt (** [b]. [b] true if there is a port side constraint *)
	| Mg
	| Next of string
	| ResetDer
	| RecordPat
	| InnerRecordPat of string 
	| Record
	| InnerRecord of string
	| Init of string
	| Disc
	| Test
	| InvState
	| App of int
	| PartApp of string * int * int (**[s,n1,n2] [s] is the name [n1] the number of arguments [n2] the number of given arguments *)
	| Mod 
	| Forall
	| Empty

type portLook = Invisible | Visible | Not | Question | Bubble | HalfCircle | Loop
type portSide = East | West | North | South | Undefined | Input | Output | Control

type port_type = {look : portLook ; side : portSide}

type edge_type = 
	Simple | Mult | Big | Aut_port | Aut_begin | Aut_end | Aut_begin_history | Aut_end_history 
	| Aut_first_half | Aut_first_half_begin | Aut_second_half_begin | Aut_second_half_history | Aut_second_half_end
	| Seq | Seq_half | DepLink | DepAutLink | Link | AutLink | Dash | Dot


type label_placement = Tail | Center | Head | Undef

let default_port_type = {look = Invisible; side = Undefined}

let id_cnt = ref 0

class iInformation = object
	val mutable inCycle = false
	val mutable isDead = false
	val mutable asText = false
	val mutable textContent = ""
	val mutable layer = 0

	method getInCycle = inCycle
	method setInCycle b = inCycle <- b
	method getDead = isDead
	method setDead b= isDead <- b
	method getLayer = layer

	method getAsText = asText
	method setAsText b = asText <- b
	method getTextContent = textContent
	method setTextContent t = textContent <- t
	method setLayer l = layer <- l
end 

class iEdgeLabel s = object
	val mutable name = (s : string)
	val mutable position = Undef
	val mutable forced_position = Undef
	val mutable inlined = false

	method setName s = name <- s
	method getName = name
	method getInlined = inlined

	method getPosition = 
		match forced_position with
		| Undef -> position
		| _ -> forced_position
	
	method setPosition p = position <- p
	method setForcedPosition p = forced_position <- p
	method setInlined i = inlined <- i
end

class iElement = object
	inherit iInformation
	val id = 
		let i = !id_cnt in
		incr id_cnt;
		i
	method getId = id
end


and iEndPoint n p = object
	val mutable node = (n : iNode)
	val mutable port = (p : iPort)
	val mutable var = false
	val mutable labels = ([] : iEdgeLabel list)

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
	
	method delSelf = 
		node#delPort port
end

and iEdgeContainer = object
	inherit iElement
	
	val mutable edges = ([] : iEdge list)
	val mutable back_edges = ([] : iEdge list)


	method getEdges = List.rev edges
	method getBackEdges = List.rev back_edges
	method addEdge e = edges <- e :: edges
	method addBackEdge e = back_edges <- e :: back_edges
	method setEdges el = edges <- List.rev el
end

and iNode = object(self)
	inherit iEdgeContainer

	val mutable n_type = Buffer
	val mutable children = ([] : iNode list)
	val mutable inputs = ([] : iOuterPort list)
	val mutable outputs = ([] : iOuterPort list)
	val mutable control = ([] : iOuterPort list)
	val mutable ports = ([] : iPort list)
	val mutable forceOrder = false
	val mutable enough = false
	val mutable insideSelf = false

	method getType = n_type
	method getChildren = List.rev children
	method getInputs = List.rev inputs
	method getOutputs = List.rev outputs
	method getControl = List.rev control
	method getPorts = List.rev ports
	method isForcedOrder = forceOrder
	method isForcedEnoughSize = enough
	method getInsideSelf = insideSelf

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
	method setForceOrder b = forceOrder <- b
	method setEnoughSize b = enough <- b
	method setInsideSelf b = insideSelf <- b

	method addFirstControlList l = control <- control @ (List.rev l)

	method delPort p_del =
		ports <- List.filter (fun p -> p#getId <> p_del#getId) ports;
		inputs <- List.filter (fun outer -> outer#getPort#getId <> p_del#getId) inputs;
		outputs <- List.filter (fun outer -> outer#getPort#getId <> p_del#getId) outputs;
		control <- List.filter (fun outer -> outer#getPort#getId <> p_del#getId) control
	method delOuterPort (outer : iOuterPort) = 
		self#delPort outer#getPort
end

and iPort node = object
	inherit iEdgeContainer

	val mutable name = ""
	val mutable p_type = (default_port_type : port_type)
	val mutable parent = (node : iNode)
	val mutable ofs = (None : float option)
	val mutable index = 0

	method getName = name
	method getType = p_type
	method getParent = parent
	method getOffset = ofs
	method getIndex = index

	method setName n = name <- n
	method setType t = p_type <- t
	method setParent p = parent <- p
	method setOffset o = ofs <- Some o
	method setIndex i = index <- i
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
	val mutable labels = ([] : iEdgeLabel list)
	val mutable e_type = Simple
	val mutable target = (None : iNode option)
	val mutable targetPort = (None : iPort option)
	val mutable source = (None : iNode option)
	val mutable sourcePort = (None : iPort option)
	val mutable direction = 0
	val mutable insideSelf = false

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
	method getDirectionPrio = direction
	method getInsideSelf = insideSelf

	method addLabel l = labels <- l :: labels
	method resetLabel = labels <- []
	method setType t = e_type <- t
	method setTarget t = target <- Some t
	method setTargetPort p = targetPort <- Some p
	method setSource s = source <- Some s
	method setSourcePort p = sourcePort <- Some p
	method setDirectionPrio p = direction <- p
	method setInsideSelf b = insideSelf <- b
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
