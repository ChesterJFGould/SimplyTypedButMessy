let nextToken (inChannel : in_channel) : Tokens.t option =
	try
		Some (Tokens.fromString (input_line inChannel))
	with End_of_file -> None



let unexpectedToken (token : Tokens.t) (expected : string) : string =
	Printf.sprintf "Unexpected token %S, expected %s" (Tokens.toString token) expected

let unexpectedEOF (expected : string) : string =
	Printf.sprintf "Unexpected EOF, expected %s" expected



let parseTypeQualifier (tq : TokenQueue.t) : Nodes.kind =
	let rec parseType () : Nodes.kind =
		match TokenQueue.next tq with
		| Some (LParen _) ->
			let l = parseType ()
			in
			begin match TokenQueue.next tq with
			| Some (RParen _) -> ()
			| Some t -> raise (Failure (unexpectedToken t "RParen"))
			| None -> raise (Failure (unexpectedEOF "RParen"))
			end;
			begin match tq.peek with
			| Some (Arrow _) ->
				ignore (TokenQueue.next tq);
				Function (l, parseType ())
			| _ -> l
			end
		| Some (Identifier (n, _)) ->
			begin match tq.peek with
			| Some (Arrow _) ->
				ignore (TokenQueue.next tq);
				Function (Base n, parseType ())
			| _ -> Base n
			end
		| Some t -> raise (Failure (unexpectedToken t "type"))
		| None -> raise (Failure (unexpectedEOF "type"))
	in
	begin match TokenQueue.next tq with
	| Some (Colon _) -> ()
	| Some t -> raise (Failure (unexpectedToken t "type qualifier"))
	| None -> raise (Failure (unexpectedEOF "type qualifier"))
	end;
	parseType ()



let rec parse (tq : TokenQueue.t) (env : (string * Nodes.kind) list) : Nodes.t option =
	match parseNode tq env with
	| Some n -> Some (parseApplication n tq env)
	| None -> None
and parseApplication (left : Nodes.t) (tq : TokenQueue.t) (env : (string * Nodes.kind) list) : Nodes.t =
	match parseNode tq env with
	| Some right ->
		begin match Nodes.tKind left with
		| Function (_, k) -> parseApplication (Application (left, right, k, Nodes.tLocation left)) tq env
		| Base _ -> raise (Failure (Printf.sprintf "Cannot parse application of non-function type %S at %s" (Nodes.kindToString (Nodes.tKind left)) (Location.toString (Nodes.tLocation left))))
		end
	| None -> left
and parseNode (tq : TokenQueue.t) (env : (string * Nodes.kind) list) : Nodes.t option =
	match tq.peek with
	| Some (Lambda _) -> Some (parseAbstraction tq env)
	| Some (Identifier _) -> Some (parseVariable tq env)
	| Some (LParen l) ->
		ignore (TokenQueue.next tq);
		let n = begin match parseNode tq env with
		| Some n -> Some (parseApplication n tq env)
		| None -> None
		end
		in begin match TokenQueue.next tq with
		| Some (RParen _) -> n
		| Some t -> raise (Failure (unexpectedToken t (Printf.sprintf "RParen matching %S" (Location.toString l))))
		| None -> raise (Failure (unexpectedEOF (Printf.sprintf "RParen matching %S" (Location.toString l))))
		end
	| _ -> None
and parseVariable (tq : TokenQueue.t) (env : (string * Nodes.kind) list) : Nodes.t =
	match tq.peek with
	| Some (Identifier (n, l)) ->
		begin match List.find_opt (fun (v, _) -> v = n) env with
		| Some (_, t) ->
			ignore (TokenQueue.next tq);
			Variable (n, t, l)
		| None ->
			let (v, k, l) = parseVariableType tq
			in
			Variable (v, k, l)
		end
	| Some t -> raise (Failure (unexpectedToken t "Identifier"))
	| None -> raise (Failure (unexpectedEOF "Identifier"))
and parseVariableType (tq : TokenQueue.t) : string * Nodes.kind * Location.t =
	match TokenQueue.next tq with
	| Some (Identifier (n, l)) -> (n, parseTypeQualifier tq, l)
	| Some t -> raise (Failure (unexpectedToken t "Identifier"))
	| None -> raise (Failure (unexpectedEOF "Identifier"))
and parseAbstraction (tq : TokenQueue.t) (env : (string * Nodes.kind) list) : Nodes.t =
	begin match TokenQueue.next tq with
	| Some (Lambda _) -> ()
	| Some t -> raise (Failure (unexpectedToken t "Lambda"))
	| None -> raise (Failure (unexpectedEOF "Lambda"))
	end;
	let (v, k, l) = parseVariableType tq
	in
	begin match TokenQueue.next tq with
	| Some (Dot _) -> ();
	| Some t -> raise (Failure (unexpectedToken t "Dot"))
	| None -> raise (Failure (unexpectedEOF "Dot"))
	end;
	match parse tq ((v, k)::env) with
	| Some n -> Abstraction (v, n, Function (k, Nodes.tKind n), l)
	| None -> raise (Failure (unexpectedEOF "lambda body"))



let () =
	if Array.length Sys.argv > 1 then
		for i = 1 to Array.length Sys.argv - 1 do
			let inChannel = open_in Sys.argv.(i)
			in
			match parse (TokenQueue.create inChannel) [] with
			| Some n -> Nodes.print n
			| None -> ()
		done
	else
		match parse (TokenQueue.create stdin) [] with
		| Some n -> Nodes.print n
		| None -> ()
