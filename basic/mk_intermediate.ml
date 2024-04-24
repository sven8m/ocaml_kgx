open Intermediate_graph



let rec create_n_ports ?(no=false) ?(vis=false) n node ty = 
	match n with
	| 0 -> []
	| n -> 
		let port = new iPort node in
		port#setType ty;
		port#setVisible vis;
		port#setNot no;
		node#addPort port;
		let outer = new iOuterPort port in
		outer :: (create_n_ports ~no:no ~vis:vis (n-1) node ty)

let outerToEndPoint o = 
	let endPoint = new iEndPoint (o#getNode) (o#getPort) in
	endPoint

(** returns number of inputs , outputs , control *)
let number_ports node_type = match node_type with
	| Concat | Nand  -> 2,1,0
	| Slice _ | Select _ | Reg | Buffer | Not -> 1,1,0
	| Fby -> 1,1,1
	| Mux -> 2,1,1
	| Cond n -> (n+1) , 1 , n (*to check*)
	| Match l -> List.length l , 1, 1
	| And n | Or n | Xor n | Tuple n -> n , 1 , 0
	| UnTuple n -> 1 , n , 0
	| Var _ | Const _ -> 0, 1 , 0
	| Sink _ -> 1,0,0
	| Rom -> 1,1,0
	| Ram -> 4,1,0
	| Pause _ -> 0,0,0
	| Sync _ -> 0,0,0
	| For -> 0,0,0
	| While -> 0,0,0
	| Final -> 0,0,0
	| Every _ | Fct _ | Match_node | Match_state _ | Reset | Aut | Aut_state _ -> assert false

let is_output_not node_type = match node_type with
	| Nand | Not -> true
	| _ -> false


let addOuterNames outer_list name_list = 
	List.iter2 (fun outer name ->
		outer#getPort#setName name) outer_list name_list

let addOffsets outer_list ofs_list = 
	List.iter2 (fun outer ofs ->
		outer#getPort#setOffset ofs) outer_list ofs_list

let addNames node node_type = 
	match node_type with
	| Match l -> addOuterNames node#getInputs l;
		addOffsets node#getControl [-.3.0 -. 3.0 *. (float_of_int (List.length l))]
	| Mux -> addOuterNames node#getInputs ["0";"1"];
		addOffsets node#getControl [-3.5]
	| Cond n ->
		let conds = List.init n (fun i -> string_of_int (i+1)) in 
		let inputs = List.init (n+1) (fun i -> if i <> n then string_of_int (i+1) else "else") in
		addOuterNames node#getControl conds;
		addOuterNames node#getInputs inputs;
		let offsets = List.init n (fun i ->
			let i = i + 1 in
			let delta = if i >= 10 then
				8.0 -. 0.013 *. (float_of_int n)
			else 8.0 -. 0.045 *. (float_of_int n)
			in
			let ofs = if i >= 10 then (-5.0) -. 0.013 *. (float_of_int n) else 4.0 in
			(-.ofs -. delta *. (float_of_int (i - 1)))
		) in
		addOffsets node#getControl offsets
	| Ram -> 
		addOuterNames node#getInputs ["read_addr";"write?";"write_addr";"write_data"]
	| _ -> ()
let simpleOpNode node_type layer =
	let nb_inputs,nb_outputs, nb_control = number_ports node_type in
	let node = new iNode in
	node#setType node_type;
	node#setLayer layer;
	node#addInputList (create_n_ports nb_inputs node Input);
	node#addOutputList (create_n_ports ~no:(is_output_not node_type) nb_outputs node Output);
	node#addControlList (create_n_ports ~vis:true nb_control node Control);
	addNames node node_type;
	node

let simpleFunctionNode ?(control=false) node_type input_names output_names layer = 
	let node = new iNode in
	node#setType node_type;
	node#setLayer layer;
	node#addInputList (create_n_ports ~vis:true (List.length input_names) node Input);
	node#addOutputList (create_n_ports ~vis:true (List.length output_names) node Output);
	if control then node#addControlList (create_n_ports ~vis:true 1 node Control); 
	addOuterNames node#getInputs input_names;
	addOuterNames node#getOutputs output_names;
	node

let addReset node = 
	node#addControlList (create_n_ports ~vis:true 1 node Control)

let new_edge edge_type (source : iEndPoint) target = 
	let edge = new iEdge in
	edge#setType edge_type;
	edge#setTarget target#getNode;
	edge#setTargetPort target#getPort;
	List.iter (fun l ->
		edge#addLabel (l , Tail)) source#eatLabels;
	List.iter (fun l ->
		edge#addLabel (l, Head)) target#eatLabels;
	(source#getPort)#addEdge edge

let automaton_edge_type reset beginning = 
	match reset , beginning with
	| true , true -> Aut_begin
	| true, false -> Aut_end
	| false , true -> Aut_begin_history
	| false, false -> Aut_end_history

let automaton_edge source target lab reset beginning = 
	let edge_type = automaton_edge_type reset beginning in
	let edge = new iEdge in
	edge#setType edge_type;
	edge#setTarget target;
	edge#addLabel (lab , Center);
	source#addEdge edge

let seq_edge ?(half=false) ?(sourcePort=None) ?(targetPort=None) sourceNode targetNode label = 
	let edge_type = if half then Seq_half else Seq in
	let edge = new iEdge in
	edge#setType edge_type;
	edge#setTarget targetNode;
	edge#addLabel (label, Center);
	begin match targetPort with
	| None -> ()
	| Some p -> edge#setTargetPort p
	end;
	match sourcePort with
	| None -> sourceNode#addEdge edge
	| Some (p : iPort) -> p#addEdge edge

