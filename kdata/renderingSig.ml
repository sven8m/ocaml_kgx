open Styles
open Placement
open Container

class type dataSig = object
	inherit PersistentEntry.propertyHolderSig
	val mutable contRenderings : containerRenderingSig list
	val mutable placement : placement option
	val mutable childArea : dataSig option
	val mutable styles : style list
	val mutable actions : Actions.action list
	val mutable junctions : containerRenderingSig list

	method addContainerRendering : containerRenderingSig -> unit 
	method getContainerRenderings : containerRenderingSig list

	method getPlacement : placement option
	method setPlacement : placement -> unit 

	method getChildArea : dataSig option
	method setChildArea : dataSig -> unit

	method addStyle : style -> unit 
	method getStyles : style list

	method addAction : Actions.action -> unit 
	method getActions : Actions.action list

	method getJunctions : containerRenderingSig list
	method addJunction : containerRenderingSig -> unit
end

and containerRenderingSig = object
	inherit dataSig
	val mutable container : container 

	method setContainer : container -> unit
	method getContainer : container
end

