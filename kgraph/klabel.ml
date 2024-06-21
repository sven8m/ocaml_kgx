open ObjectSig

class klabel graph : klabelSig = object
	inherit Kelement.kelement graph
	inherit Object_pos.obj_pos
	val mutable text = ""
	
	method getText = text
	method setText t = text <- t
 end

