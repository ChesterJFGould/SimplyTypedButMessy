type t = {
	mutable peek : char option;
	inChannel : in_channel;
	location : Location.t;
}



let next (cq : t) : char option =
	let current = cq.peek
	in
	try
		begin
		cq.peek <- Some (input_char cq.inChannel);
		match current with
		| Some '\n' ->
			begin
			cq.location.line <- cq.location.line + 1;
			cq.location.column <- 1;
			end
		| Some _ -> cq.location.column <- cq.location.column + 1;
		| None -> ();
		end;
		current
	with End_of_file ->
		begin
		cq.peek <- None;
		None
		end



let create (inChannel : in_channel) (fileName : string) : t =
	let cq = ({
		peek = None;
		inChannel = inChannel;
		location = Location.create fileName;
	} : t)
	in
	ignore (next cq);
	cq
