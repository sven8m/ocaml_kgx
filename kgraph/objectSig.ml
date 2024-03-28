open RenderingSig
class type kelementSig = object
	inherit PersistentEntry.propertyHolderSig
	val id : int
	val kgraph_id : int
	val mutable kgraph : kgraphSig
	val mutable labels : Label.label list
	val mutable data : containerRenderingSig list	
	method getId : int

	method getGraphId : int
	method getGraph : kgraphSig

	method getLabels : Label.label list
	method addLabel : Label.label -> unit

	method getData : containerRenderingSig list
	method addData : containerRenderingSig -> unit
end

and knodeSig = object 
	inherit kelementSig
	inherit Object_pos.obj_pos
	val mutable parent : knodeSig option
	val mutable children : knodeSig list
	val mutable incomingEdges : kedgeSig list
	val mutable outgoingEdges : kedgeSig list
	val mutable ports : kportSig list
	val mutable path : string

	method delChild : knodeSig -> unit
	method addChild : knodeSig -> unit
	method getChildren : knodeSig list
	
	method getParent : knodeSig option
	method setParent : knodeSig -> unit

	method getIncomingEdges : kedgeSig list
	method addIncomingEdge : kedgeSig -> unit
	method delIncomingEdge : kedgeSig -> unit
	
	method getOutgoingEdges : kedgeSig list
	method addOutgoingEdge : kedgeSig -> unit
	method delOutgoingEdge : kedgeSig -> unit

	method getPorts : kportSig list
	method addPort : kportSig -> unit
	method delPort : kportSig -> unit
	
	method getPath : string
	method setPath : string -> unit

end

and kedgeSig = object 
	inherit kelementSig
	val mutable source : knodeSig option
	val mutable target : knodeSig option
	val mutable sourcePort : kportSig option
	val mutable targetPort : kportSig option

	method setSource : knodeSig -> unit
	method getSource : knodeSig option
	method getSourceOpt : unit -> knodeSig

	method setTarget : knodeSig -> unit
	method getTarget : knodeSig option
	method getTargetOpt : unit -> knodeSig

	method getSourcePort : kportSig option
	method setSourcePort : kportSig -> unit

	method getTargetPort : kportSig option
	method setTargetPort : kportSig -> unit
end

and kportSig = object
	inherit kelementSig
	inherit Object_pos.obj_pos
	val mutable node : knodeSig option

	method getNode : knodeSig option
	method getNodeOpt : unit -> knodeSig

	method setNode : knodeSig -> unit
end

and kgraphSig = object
	inherit PersistentEntry.propertyHolderSig
	val id : int 
	val mutable cnt : int
	val mutable nodes : knodeSig list
	
	method getId : int

	method getCnt : int
	method addOne : unit -> int

	method addNode : knodeSig -> unit
	method getNodes : knodeSig list
	method delNode : knodeSig -> unit
end
