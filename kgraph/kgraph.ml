open ObjectSig

let graph_cnt = ref 0

class kgraph : kgraphSig = object
	val id = incr graph_cnt; !graph_cnt 
	val mutable cnt = 0
	val mutable nodes = [] 

	method getId = id

	method getCnt  = cnt
	method addOne ()  = 
		let v = cnt in 
		cnt <- cnt + 1;
		v
	
	method getNodes = nodes
	method delNode node = 
		nodes <- List.filter (fun n -> n#getId <> node#getId) nodes
	method addNode node = 
		nodes <- node :: nodes	
end

