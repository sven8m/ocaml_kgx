open Kgraph_objects
open Kgraph_functions



let print_kedge ff kedge =
	Format.fprintf ff "<outgoingEdges target=\"%s\">@," (getTarget kedge).path;

	Format.fprintf ff "</outgoingEdges>"

let rec print_knode ff knode =
	
	let rec compute_incomingList edge_list = 
		match edge_list with
		| [] -> ""
		| edge :: q ->
			let res = compute_incomingList q in
			let name = (getSource edge).path ^ "/@outgoingEdges." in
			let rec find_id i (el : kedge list) =
				match el with
				| [] -> failwith "edge not found"
				| e :: q -> if e.id = edge.id then i else find_id (i + 1) q
			in
			let id = find_id 0 (getSource edge).outgoingEdges in
			res ^ " " ^ name ^ (string_of_int id)	
	in
	let incoming = match (compute_incomingList knode.incomingEdges) with
	| "" -> ""
	| s -> "incomingEdges=\""^s^"\""
	in
	Format.fprintf ff "<children %s>@[<v 0>@," incoming;
	List.iter (print_knode ff) knode.children;

	List.iter (print_kedge ff) knode.outgoingEdges;
	Format.fprintf ff "@]@,</children>@,"


let graph_to_kgx ff kgraph = 
	let header = "<?xml version=\"1.0\" encoding=\"ASCII\"?>
<klighdgraph:KNode xmi:version=\"2.0\" xmlns:xmi=\"http://www.omg.org/XMI\"
    xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
    xmlns:klighdgraph=\"http://kieler.cs.cau.de/KlighdGraph\"
    xmlns:krendering=\"http://kieler.cs.cau.de/KRendering\">" in

	let finish = "</klighdgraph:KNode>" in
	
	Compute_path.compute_path_kgraph kgraph;

	Format.fprintf ff "%s@[<v 0>@," header;
	List.iter (Format.fprintf ff "%a" print_knode) kgraph.node_list;
	Format.fprintf ff "@]%s@." finish;
