
type action = {
	actionId : string;
	trigger : string option;
}

let create_action ?(trig) id = 
	{actionId = id ; trigger = trig}

let create_actionCollapse () = 
	create_action ~trig:"DOUBLECLICK" "de.cau.cs.kieler.klighd.actions.CollapseExpandAction" 

let print_action ff action = 
	Format.fprintf ff "@,<actions actionId=\"%s\"" action.actionId;
	begin match action.trigger with
	| None -> ()
	| Some t -> Format.fprintf ff " trigger=\"%s\"" t;
	end;
	Format.fprintf ff "/>"
