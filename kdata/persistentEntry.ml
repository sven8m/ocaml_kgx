type property = {
	key : string;
	value : string
}

let create_property k v = 
	{key = k ; value = v}

let createPortWest () = 	
	{key = "org.eclipse.elk.port.side" ; value="WEST"}

let createPortEast () = 
	{key = "org.eclipse.elk.port.side" ; value = "EAST"}

let constraintPortSide () = 
	{key = "org.eclipse.elk.portConstraints" ; value="FIXED_SIDE"}

let print_property ff p = 
	Format.fprintf ff "<persistentEntries key=\"%s\" value=\"%s\"/>@," p.key p.value
