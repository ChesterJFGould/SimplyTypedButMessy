exception TypeError of string



let rec substitute (sub : Nodes.t) (var : string) (node : Nodes.t) : Nodes.t =
	match node with
	| Variable (v, k, l) ->
		begin match (v = var, k = (Nodes.tKind sub)) with
		| (true, true) -> sub
		| (false, _) -> Variable (v, k, l)
		| (true, false) ->
			raise (TypeError (Printf.sprintf "Cannot substitute \"%s\" type \"%s\" for \"%s\" type \"%s\""
				(Nodes.toString sub) (Nodes.kindToString (Nodes.tKind sub))
				(Nodes.toString (Variable (v, k, l))) (Nodes.kindToString k)))
		end
	| Abstraction (v, b, k, l) ->
		if v = var then
			Abstraction (v, b, k, l)
		else
			Abstraction (v, substitute sub var b, k, l)
	| Application (l, r, k, loc) -> Application (substitute sub var l, substitute sub var r, k, loc)
	| Environment (v, s, b, k, l) ->
		if v = var then
			Environment (v, substitute sub var s, b, k, l)
		else
			Environment (v, substitute sub var s, substitute sub var b, k, l)



let rec betaReduce (node : Nodes.t) : Nodes.t =
	match node with
	| Application (left, right, _, l) ->
		begin match (Nodes.tKind left) with
		| Base k -> raise (TypeError (Printf.sprintf "Cannot use \"%s\" type \"%s\" as Abstraction in Application, %s" 
			(Nodes.toString left) k (Location.toString l)))
		| Function (f, _) ->
			if f = (Nodes.tKind right) then
				()
			else
				raise (TypeError (Printf.sprintf "Cannot use \"%s\" type \"%s\" as type \"%s\" in \"%s\", %s"
					(Nodes.toString right) (Nodes.kindToString (Nodes.tKind right))
					(Nodes.kindToString f) (Nodes.toString left) (Location.toString l)))
		end;
		begin match betaReduce left with
		| Abstraction (v, b, k, l) -> betaReduce (Environment (v, right, b, k, l))
		| n -> Application (left, betaReduce right, Nodes.tKind n, Nodes.tLocation n)
		end
	| Environment (v, sub, b, _, _) -> betaReduce (substitute sub v b)
	| n -> n



let () : unit =
	if Array.length Sys.argv > 1 then
		for i = 1 to Array.length Sys.argv - 1 do
			print_endline (Nodes.toString (betaReduce (Nodes.fromChannel (open_in Sys.argv.(i)))))
		done
	else
		print_endline (Nodes.toString (betaReduce (Nodes.fromChannel stdin)))
