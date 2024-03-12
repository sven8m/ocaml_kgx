open ObjectSig
open Kelement

class kport graph : kportSig = object (self)
	inherit kelement graph
	inherit Object_pos.obj_pos
	val mutable node = (None : knodeSig option)

	method getNode = node
	method getNodeOpt () = 
		match node with
		| None -> Format.printf "sob@.";new Knode.knode kgraph
		| Some n -> n

	method setNode (n : knodeSig) = 
		if n#getGraphId <> kgraph_id then
			failwith "not in same kgraph";
		begin match node with
		| None -> ()
		| Some n ->
			n#delPort (self :> kport)
		end;
		node <- Some n;
		n#addPort (self :> kport)

end

