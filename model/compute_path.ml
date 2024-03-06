open Kgraph_objects


let rec compute_path_knode knode path id =
	let path = path^"/@children."^(string_of_int id) in
	knode.path <- path;
	List.iteri (fun i child -> compute_path_knode child path i) knode.children


let compute_path_kgraph kgraph = 
	let path = "/" in
	List.iteri (fun i child -> compute_path_knode child path i) kgraph.node_list

