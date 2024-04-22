(*open Styles
*)
(* type *)
type container = 
	| Rect 
	| RoundRect of float * float
	| Spline
	| PolyLine of Point.point list
	| Text of string
	| Ellipse
	| Polygon of Point.point list
	| Default
	| RoundPolyline of float * Point.point list
(* creation *)

(* functions *)

(* printing *)

let container_to_string c =
	match c with
	| Rect -> "KRectangle"
	| RoundRect _ -> "KRoundedRectangle"
	| Spline -> "KSpline"
	| Text _ -> "KText"
	| PolyLine _ -> "KPolyline"
	| Ellipse -> "KEllipse"
	| Polygon _ -> "KPolygon"
	| Default -> "KRectangle"
	| RoundPolyline _ -> "KRoundedBendsPolyline"

let print_container_infos ff c = 
	match c with
	| Rect -> ()
	| RoundRect (x,y) -> Format.fprintf ff " cornerWidth=\"%f\" cornerHeight=\"%f\"" x y
	| Spline -> ()
	| Text t -> Format.fprintf ff " text=\"%s\"" t
	| PolyLine _ -> ()
	| Ellipse -> ()
	| Default -> ()
	| Polygon _ -> ()
	| RoundPolyline (x,_) -> Format.fprintf ff " bendRadius=\"%f\"" x

let print_container_content ff c = 
	match c with
	| RoundPolyline (_,pl) | PolyLine pl | Polygon pl ->
		List.iter (fun point ->
			Format.fprintf ff "@,@[<v 4><points>%a@]@,</points>" Point.print_point point) pl
	| _ -> ()
	
let print_container ff c =
	Format.fprintf ff "%s\"%a" (container_to_string c) print_container_infos c	
