open ObjectSig

class kelement graph : kelementSig = object
	val id = graph#addOne () 
	val kgraph_id = graph#getId 
	val mutable kgraph = graph
	val mutable labels = []
	val mutable container = Container.Default
	val mutable data = []
	val mutable properties = []

	
	method getId = id

	method getGraphId = kgraph_id
	method getGraph = graph

	method getLabels = labels
	method addLabel label = labels <- label :: labels

	method setContainer c = 
		container <- c
	method getContainer = container

	method addData d = 
		data <- d :: data
	method getData = data

	method getProperties = properties
	method addProperty p = properties <- p :: properties

end
