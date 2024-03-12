(*
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

	let node4 = create_knode kgraph in
	setParent node1 node4;

	let edge2 = create_kedge kgraph in
	setSource edge2 node2;
	setTarget edge2 node4;

	let edge3 = create_kedge kgraph in
	setSource edge3 node2;
	setSource edge3 node3;
	setTarget edge3 node4;

	let edge4 = create_kedge kgraph in
	setSource edge4 node4;
	setTarget edge4 node3;
	
	let text1 = create_text "hi" in
	let label1= create_label ~position:CENTER text1 in

	addLabeltoNode node1 label1;
	addLabeltoEdge edge4 label1;
	Kgraph_printer.graph_to_kgx ff kgraph
*)	

let main () =
		Arg.parse [] Fulladder.main "";;

let () = main ()

