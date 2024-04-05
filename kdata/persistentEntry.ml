type property = {
	key : string;
	value : string
}

let create_property k v = 
	{key = k ; value = v}


class type propertyHolderSig = object
	val mutable properties : property list

	method getProperties : property list
	method addProperty : property -> unit
end

class propertyHolder : propertyHolderSig = object
	val mutable properties = []

	method getProperties = List.rev properties
	method addProperty p =
		properties <- p :: properties
end


let createPortWest () = 	
	{key = "org.eclipse.elk.port.side" ; value="WEST"}

let createPortEast () = 
	{key = "org.eclipse.elk.port.side" ; value = "EAST"}

let createPortNorth () = 
	{key = "org.eclipse.elk.port.side" ; value = "NORTH"}

let constraintPortSide () = 
	{key = "org.eclipse.elk.portConstraints" ; value="FIXED_SIDE"}
let constraintPortOrder () = 
	 {key = "org.eclipse.elk.portConstraints" ; value="FIXED_ORDER"}

let allowSwitch () = 
	{key = "org.eclipse.elk.layered.allowNonFlowPortsToSwitchSides" ; value="true"}

let portLabelPlacement s = 
	{key = "org.eclipse.elk.portLabels.placement" ; value=s}

let edgeLabelPlacement s = 
	{key = "org.eclipse.elk.edgeLabels.placement" ; value=s}

let nodeSize s = 
	{key = "org.eclipse.elk.nodeSize.constraints" ; value=s}

let expand b = 
	{key = "de.cau.cs.kieler.klighd.expand" ; value = b}

let addPortSpace top bot left right = 
	{key = "org.eclipse.elk.spacing.portsSurrounding"  ;value= "[top="^top^",left="^left^",bottom="^bot^",right="^right^"]"}

let portAlignmentNorth s = 
	{key = "org.eclipse.elk.portAlignment.north" ; value = s}

let portAlignmentSouth s = 
	{key = "org.eclipse.elk.portAlignment.south"; value = s}

let partition v = 
	{key = "org.eclipse.elk.partitioning.partition"; value = v}

let activatePartition () = 
	{key = "org.eclipse.elk.partitioning.activate" ; value = "true"}

let borderOffset v = 
	{key = "org.eclipse.elk.port.borderOffset" ; value = string_of_float v}

let print_property ff p = 
	Format.fprintf ff "<persistentEntries key=\"%s\" value=\"%s\"/>@," p.key p.value

let print_properties ff p = 
	List.iter (fun p -> print_property ff p) p#getProperties
