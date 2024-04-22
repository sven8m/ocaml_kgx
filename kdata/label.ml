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

end

let print_label ff label = 
	Format.fprintf ff "@,@[<v 4><labels text=\"%s\">" label#getText;
	List.iter (PersistentEntry.print_property ff) label#getProperties;
	Format.fprintf ff "@,@[<v 4><data xsi:type=\"krendering:KText\">";
	List.iter (print_style ff) label#getStyles;
	Format.fprintf ff "@]@,</data>@]";
	Format.fprintf ff "@,</labels>"

