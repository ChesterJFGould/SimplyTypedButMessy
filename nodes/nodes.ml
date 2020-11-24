type kind =
	| Base of string
	| Function of kind * kind

let rec kindToString (k : kind) : string =
	match k with
	| Base n -> n
	| Function (f, t) -> Printf.sprintf "(%s->%s)" (kindToString f) (kindToString t)

let stringToKind (s : string) : kind =
	let explode (s : string) : char list = List.init (String.length s) (String.get s)
	in let rec identifier (s : char Stream.t) (ident : string) : string =
		match Stream.npeek 2 s with
		| [] -> ident
		| ')'::_ ->
			Stream.junk s;
			ident
		| ['-'; '>'] -> ident
		| c::_ ->
			Stream.junk s;
			identifier s (ident ^ String.make 1 c)
	in let rec parse (s : char Stream.t) : kind =
		match Stream.npeek 2 s with
		| '('::_ ->
			Stream.junk s;
			let l = parse s
			in
			let r = parse s
			in
			Function (l, r)
		| ['-'; '>'] ->
			Stream.junk s;
			Stream.junk s;
			parse s
		| ')'::_ ->
			Stream.junk s;
			parse s
		| _ -> Base (identifier s "")
	in parse (Stream.of_list (explode s))



type t =
	| Variable of string * kind * Location.t
	| Abstraction of string * t * kind * Location.t
	| Application of t * t * kind * Location.t
	| Environment of string * t * t * kind * Location.t

let tKind (node : t) : kind =
	match node with
	| Variable (_, k, _) -> k
	| Abstraction (_, _, k, _) -> k
	| Application (_, _, k, _) -> k
	| Environment (_, _, _, k, _) -> k



let tLocation (node : t) : Location.t =
	match node with
	| Variable (_, _, l) -> l
	| Abstraction (_, _, _, l) -> l
	| Application (_, _, _, l) -> l
	| Environment (_, _, _, _, l) -> l



let rec print (node : t) : unit =
	match node with
	| Variable (n, k, l) -> Printf.printf "Variable %s %s %s\n" n (kindToString k) (Location.toString l)
	| Abstraction (v, b, k, l) ->
		begin
		Printf.printf "Abstraction %s %s %s\n" v (kindToString k) (Location.toString l);
		print b
		end
	| Application (left, right, k, l) ->
		begin
		Printf.printf "Application %s %s\n" (kindToString k) (Location.toString l);
		print left;
		print right
		end
	| Environment (v, sub, b, k, l) ->
		begin
		Printf.printf "Environment %s %s %s\n" v (kindToString k) (Location.toString l);
		print sub;
		print b
		end



let rec fromChannel (inChannel : in_channel) : t =
	match String.split_on_char ' ' (input_line inChannel) with
	| "Variable"::n::k::tl -> Variable (n, stringToKind k, Location.fromStringList tl)
	| "Abstraction"::v::k::tl -> Abstraction (v, fromChannel inChannel, stringToKind k, Location.fromStringList tl)
	| "Application"::k::tl ->
		let l = fromChannel inChannel
		in let r = fromChannel inChannel
		in Application (l, r, stringToKind k, Location.fromStringList tl)
	| "Environment"::v::k::tl ->
		let sub = fromChannel inChannel
		in let b = fromChannel inChannel
		in Environment (v, sub, b, stringToKind k, Location.fromStringList tl)
	| s -> raise (Failure (Printf.sprintf "Cannot parse %S into Node" (String.concat " " s)))



let printTree (node : t) : unit =
	let rec printIndent (node : t) (indent : string) (last : bool) : unit =
		print_string indent;
		let newIndent = 
			if last then
				begin
				print_string "\\-";
				indent ^ "  "
				end
			else
				begin
				print_string "|-";
				indent ^ "| "
				end
		in
		match node with
		| Variable (n, k, _) -> Printf.printf "%s : %s\n" n (kindToString k)
		| Abstraction (v, b, k, _) ->
			Printf.printf "λ %s : %s\n" v (kindToString k);
			printIndent b newIndent true
		| Application (l, r, k, _) ->
			Printf.printf "@ : %s\n" (kindToString k);
			printIndent l newIndent false;
			printIndent r newIndent true
		| Environment (v, sub, b, _, _) ->
			Printf.printf "%s :=\n" v;
			printIndent sub newIndent false;
			printIndent b newIndent true
	in printIndent node "" true



let rec toString (node : t) : string =
	match node with
	| Variable (n, _, _) -> n
	| Abstraction (v, b, k, _) ->
		begin match k with
		| Function (f, _) -> Printf.sprintf "(λ%s : %s. %s)" v (kindToString f) (toString b)
		| Base k -> Printf.sprintf "λ%s : %s. %s" v k (toString b) 
		end
	| Application (l, r, _, _) -> Printf.sprintf "(%s) %s" (toString l) (toString r)
	| Environment (v, sub, b, _, _) -> Printf.sprintf "(%s) [%s := %s]" (toString b) v (toString sub)
