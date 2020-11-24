type t = {
	mutable peek : Tokens.t option;
	inChannel : in_channel;
}

let next (tq : t) : Tokens.t option =
	let current = tq.peek
	in
	try
		tq.peek <- Some (Tokens.fromString (input_line tq.inChannel));
		current
	with End_of_file ->
		tq.peek <- None;
		current

let create (inChannel : in_channel) : t =
	let tq = ({peek = None; inChannel = inChannel;} : t)
	in
	ignore (next tq);
	tq
