open Kelement
open ObjectSig

class kedge graph : kedgeSig = object (self) 
	inherit klabeledelement graph
	val mutable source = (None : knodeSig option)
	val mutable target = (None : knodeSig option)
	val mutable sourcePort = (None : kportSig option)
	val mutable targetPort = (None : kportSig option)

	method setSource node = 
		if self#getGraphId <> node#getGraphId then
			failwith "not in same kgraph";
		begin match source with
		| None -> ()
		| Some node ->
			node#delOutgoingEdge (self :> kedge)
		end;
		source <- Some node;
		node#addOutgoingEdge (self :> kedge)
	
	method getSource = source 
	method getSourceOpt () = 
		match source with
		| None -> Format.printf "qqqqqqqqqqqq@.";new Knode.knode kgraph
		| Some n -> n

	method setTarget node =
		if self#getGraphId <> node#getGraphId then
			failwith "not in same kgraph";
		begin match target with
		| None -> ()
		| Some node ->
			node#delIncomingEdge (self :> kedge)
		end;
		target <- Some node;
		node#addIncomingEdge (self :> kedge)
	
	method getTarget = target
	method getTargetOpt () = 
		match target with
		| None -> new Knode.knode kgraph
		| Some n -> n

	method getSourcePort = sourcePort
	method setSourcePort port = 
		begin match source with
		| None -> failwith "source node not defined"
		| Some node ->
			begin match port#getNode with
			| None -> failwith "port node not defined"
			| Some n ->
			if node#getId <> n#getId then failwith "source node an port are not the same"
			end
		end;
		sourcePort <- Some port
	
	method getTargetPort = targetPort
	method setTargetPort port = 
		begin match target with
		| None -> failwith "target node not defined"
		| Some node ->
			begin match port#getNode with
			| None -> failwith "port node not defined"
			| Some n ->
			if node#getId <> n#getId then failwith "source node an port are not the same"
			end
		end;
		targetPort <- Some port

end

