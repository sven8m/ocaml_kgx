open Coloring
(*type declarations *)

type line_style = SOLID | DASH | DOT | DASHDOT | DASHDOTDOT

type c_style = 
	| Shadow of float * float
	| Invisibility
	| Background of coloring
	| Foreground of coloring
	| LineWidth of float
	| Bold
	| FontSize of int
	| LineStyle of line_style
	| JoinRound

type style = {
	style : c_style;
	on_selection : bool
}

(*objects creations *)

let create_style ?(on_sel=false) style  =
	{style = style ; on_selection = on_sel}


(*objects manipulation*)


(*objects printing*)

let style_to_string c_style = match c_style with
	| Shadow _ -> "KShadow"
	| Invisibility -> "KInvisibility"
	| Background _ -> "KBackground"
	| Foreground _ -> "KForeground"
	| LineWidth _ -> "KLineWidth"
	| Bold -> "KFontBold"
	| FontSize _ -> "KFontSize"
	| LineStyle _ -> "KLineStyle"
	| JoinRound -> "KLineJoin"

let line_style_to_string s = match s with
	| SOLID -> "SOLID"
	| DASH -> "DASH"
	| DOT -> "DOT"
	| DASHDOT -> "DASHDOT"
	| DASHDOTDOT -> "DASHDOTDOT"

let print_style ff style =

	Format.fprintf ff "@[<v 4><styles xsi:type=\"krendering:%s\"" (style_to_string style.style);
	if style.on_selection then
		Format.fprintf ff " selection=\"true\"";
	begin match style.style with
	| Shadow (ofsx , ofsy) ->
		Format.fprintf ff " xOffset=\"%f\" yOffset=\"%f\"" ofsx ofsy
	| Invisibility -> ()
	| Background coloring | Foreground coloring ->
		if coloring.gradient then
			Format.fprintf ff " gradientAngle=\"%f\"" coloring.gradient_angle
	| LineWidth f ->
		Format.fprintf ff " lineWidth=\"%f\"" f
	| Bold -> ()
	| FontSize d ->
		Format.fprintf ff " size=\"%d\"" d
	| LineStyle s ->
		Format.fprintf ff " lineStyle=\"%s\"" (line_style_to_string s)
	| JoinRound ->
		Format.fprintf ff " lineJoin=\"JOIN_ROUND\""
	end;
	Format.fprintf ff ">";
	begin match style.style with
	| Background coloring | Foreground coloring ->
		print_color ff coloring.color false;
		if coloring.gradient then
			print_color ff coloring.target_color true;
	| _ -> ()
	end;
	Format.fprintf ff "@]@,</styles>@,"
