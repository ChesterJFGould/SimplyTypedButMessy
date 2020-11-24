let alphaCharset = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
let alphaNumCharset = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"



let lexIdentifier (first : char) (cq : CharQueue.t) (location : Location.t) : Tokens.t =
	let rec loop (ident : string) : string =
		match cq.peek with
		| Some c ->
			if String.contains alphaNumCharset c then
				begin
				ignore (CharQueue.next cq);
				loop (ident ^ (String.make 1 c))
				end
			else
				ident
		| None -> ident
	in
	Identifier (loop (String.make 1 first), location)



let rec lex (cq : CharQueue.t) : unit =
	let rec nextToken () : Tokens.t option =
		match CharQueue.next cq with
		| Some '\\' -> Some (Lambda cq.location)
		| Some '(' -> Some (LParen cq.location)
		| Some ')' -> Some (RParen cq.location)
		| Some '.' -> Some (Dot cq.location)
		| Some ':' -> Some (Colon cq.location)
		| Some '-' when cq.peek = Some '>' ->
			begin
			ignore (CharQueue.next cq);
			Some (Arrow cq.location)
			end
		| Some c when String.contains alphaCharset c -> Some (lexIdentifier c cq (Location.copy cq.location))
		| Some _ -> nextToken ()
		| None -> None
	in
	match nextToken () with
	| Some t ->
		begin
		print_endline (Tokens.toString t);
		lex cq;
		end
	| None -> ()


			
let () : unit =
	if Array.length Sys.argv > 1 then
		for i = 1 to Array.length Sys.argv - 1 do
			let inChannel = open_in Sys.argv.(i)
			in
			lex (CharQueue.create inChannel Sys.argv.(i))
		done
	else
		lex (CharQueue.create stdin "stdin")
