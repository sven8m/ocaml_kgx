open Kgraph_objects
open Kgraph_functions

let do_it filename =
	
	let oc = open_out filename in 
	let ff = Format.formatter_of_out_channel oc in
	let kgraph = create_kgraph () in
	let node1 = create_knode kgraph in
	let node2 = create_knode kgraph in

	let node3 = create_knode kgraph in
	setParent node1 node2;
	setParent node1 node3;

	let edge1 = create_kedge kgraph in
	setSource edge1 node2;
	setTarget edge1 node3;
	Printer.graph_to_kgx ff kgraph

let main () =
	try
		Arg.parse [] do_it "";
  with
    | _ -> exit 2;;

main ()

