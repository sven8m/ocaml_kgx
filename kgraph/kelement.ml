open ObjectSig
open Rendering

class kelement graph : kelementSig = object
	inherit PersistentEntry.propertyHolder
	val id = graph#addOne () 
	val kgraph_id = graph#getId 
	val mutable kgraph = graph
	val mutable data = []

	method getId = id

	method getGraphId = kgraph_id
	method getGraph = graph

	method getData = List.rev data
	method addData d = data <- d :: data
end

class klabeledelement graph : klabeledelementSig = object
	inherit kelement graph
	val mutable labels = []

	method getLabels = List.rev labels
	method addLabel label = labels <- label:: labels
end
