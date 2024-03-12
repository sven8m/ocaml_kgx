(*open Styles
*)
(* type *)
type container = 
	| Rect 
	| RoundRect of float * float
	| Spline
	| PolyLine
	| Text of string
	| Ellipse
	| Polygon
	| Default
(* creation *)

(* functions *)

(* printing *)

let container_to_string c =
	match c with
	| Rect -> "KRectangle"
	| RoundRect _ -> "KRoundedRectangle"
	| Spline -> "KSpline"
	| Text _ -> "KText"
	| PolyLine -> "KPolyline"
	| Ellipse -> "KEllipse"
	| Polygon -> "KPolygon"
	| Default -> "KRectangle"

let print_container_infos ff c = 
	match c with
	| Rect -> ()
	| RoundRect (x,y) -> Format.fprintf ff "cornerWidth = \"%f\" cornerHeight = \"%f\"" x y
	| Spline -> ()
	| Text t -> Format.fprintf ff "text=\"%s\"" t
	| PolyLine -> ()
	| Ellipse -> ()
	| Polygon -> ()
	| Default -> ()

let print_container ff c =
	Format.fprintf ff "%s\" %a" (container_to_string c) print_container_infos c	
