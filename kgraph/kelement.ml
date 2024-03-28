open ObjectSig
open Rendering

class kelement graph : kelementSig = object
	inherit PersistentEntry.propertyHolder
	val id = graph#addOne () 
	val kgraph_id = graph#getId 
	val mutable kgraph = graph
	val mutable labels = []
	val mutable data = []

	method getId = id

	method getGraphId = kgraph_id
	method getGraph = graph

	method getLabels = List.rev labels
	method addLabel label = labels <- label :: labels

	method getData = List.rev data
	method addData d = data <- d :: data
end
