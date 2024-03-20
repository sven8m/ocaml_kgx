open Label
open Rendering
open Container

open Kelement
open Kport
open Knode
open Kgraph
open Kedge
open PersistentEntry

let print_port ff port = 
	Format.fprintf ff "@[<v 4><ports %a>@," Object_pos.print_obj_pos (port :> Object_pos.obj_pos) ;
	List.iter (print_property ff) port#getProperties;
	List.iter (print_label ff) port#getLabels;
	List.iter (print_data_node ff) port#getData;
	Format.fprintf ff "@]@,</ports>@,"

let findPortId port node =
	Format.printf "iao : %d %d@." (node#getId) (List.length node#getPorts);
	let rec aux i pl = match pl with
	| [] -> failwith "not found"
	| p :: q -> if p#getId = port#getId then i else aux (i + 1) q
	in
	aux 0 node#getPorts

let print_sourcePort ff kedge = 
	match kedge#getSourcePort with
	| None -> ()
	| Some p ->
		let node = p#getNodeOpt () in
		Format.fprintf ff "sourcePort=\"%s/@ports.%d\"" (node#getPath) (findPortId p node)

let print_targetPort ff kedge = 
	match kedge#getTargetPort with
	| None -> ()
	| Some p ->
		let node = p#getNodeOpt () in
		Format.fprintf ff "targetPort=\"%s/@ports.%d\"" node#getPath (findPortId p node)

let print_kedge ff kedge =	
	Format.fprintf ff "@[<v 4><outgoingEdges target=\"%s\" %a %a>@," (kedge#getTargetOpt ())#getPath print_sourcePort kedge print_targetPort kedge;
	List.iter (print_property ff) kedge#getProperties;
	List.iter (print_label ff) kedge#getLabels;
	List.iter (print_data_node ff) kedge#getData;
	Format.fprintf ff "@]@,</outgoingEdges>@,"

let rec print_knode ff (knode :knode) =
	
	let rec compute_incomingList edge_list = 
		match edge_list with
		| [] -> ""
		| edge :: q ->
			let res = compute_incomingList q in
			let name = (edge#getSourceOpt ())#getPath ^ "/@outgoingEdges." in
			let rec find_id i (el : kedge list) =
				match el with
				| [] -> failwith "edge not found"
				| e :: q -> if e#getId = edge#getId then i else find_id (i + 1) q
			in
			let id = find_id 0 (edge#getSourceOpt ())#getOutgoingEdges in
			res ^ " " ^ name ^ (string_of_int id)	
	in
	let incoming = match (compute_incomingList knode#getIncomingEdges) with
	| "" -> ""
	| s -> "incomingEdges=\""^s^"\""
	in
	Format.fprintf ff "@[<v 4><children %a %s>@," Object_pos.print_obj_pos (knode :> Object_pos.obj_pos) incoming;	
	List.iter (print_property ff) knode#getProperties;
	List.iter (print_label ff) knode#getLabels;
	(*printing data*)
	List.iter (print_data_node ff) (knode#getData);
	(*printing children *)
	
	List.iter (print_knode ff) knode#getChildren;

	List.iter (print_kedge ff) knode#getOutgoingEdges;
	List.iter (print_port ff) knode#getPorts;
	Format.fprintf ff "@]@,</children>@,"


let graph_to_kgx ff (kgraph : kgraph) = 
	let header = "<?xml version=\"1.0\" encoding=\"ASCII\"?>
<klighdgraph:KNode xmi:version=\"2.0\" xmlns:xmi=\"http://www.omg.org/XMI\"
    xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
    xmlns:klighdgraph=\"http://kieler.cs.cau.de/KlighdGraph\"
    xmlns:krendering=\"http://kieler.cs.cau.de/KRendering\">" in

	let finish = "</klighdgraph:KNode>" in
	
	Compute_path.compute_path_kgraph kgraph;

	Format.fprintf ff "%s@[<v 0>@," header;
	List.iter (Format.fprintf ff "%a" print_knode) kgraph#getNodes;
	Format.fprintf ff "@]%s@." finish;

