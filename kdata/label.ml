open Styles


class label = object (self)
	inherit Object_pos.obj_pos
	val mutable text = ("" : string)
	val mutable prop = ([] : PersistentEntry.property list)
	val mutable styles = ([] : style list)

	method setText t = text <- t
	method addProperty p = prop <- p :: prop
	method addStyle s = styles <- s :: styles

	method getText = text
	method getProperties = prop
	method getStyles = styles

	method setCenter = 
		let en = PersistentEntry.create_property "org.eclipse.elk.edgeLabels.placement" "CENTER" in
		self#addProperty en
end

(*
type text = {
	mutable content : string;
	mutable styles : style list
}

type position = UNDEF | CENTER

type label = {
	mutable objPos : Object_pos.obj_pos;
 	mutable text : text;
	mutable prop : PersistentEntry.property list}
}

let create_text ?(size=11) ?(bold=true) content = 
	let t = {content = content ; styles = [create_style (FontSize size)]} in
	if bold then
		t.styles <- (create_style Bold) :: t.styles;
	t

let create_label ?(position=UNDEF) text = 
	{text = text ; position = UNDEF}

let position_to_string position = 
	match position with
	| UNDEF -> ""
	| CENTER -> "CENTER"
*)

let print_label ff label = 
	Format.fprintf ff "@[<v 4><labels text=\"%s\">@," label#getText;
	List.iter (PersistentEntry.print_property ff) label#getProperties;
	Format.fprintf ff "@[<v 4><data xsi:type=\"krendering:KText\">@,";
	List.iter (print_style ff) label#getStyles;
	Format.fprintf ff "@]@,</data>@]@,";
	Format.fprintf ff "</labels>@,"

