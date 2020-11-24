type t = {
	mutable line : int;
	mutable column : int;
	file : string;
}

let create (fileName : string) : t =
	({
		line = 1;
		column = 0;
		file = fileName;
	} : t)



let toString ({line; column; file;}: t) : string =
	Printf.sprintf "%d %d %s" line column file


let fromStringList (s : string list) : t =
	match s with
	| [line; column; file;] -> ({line = (int_of_string line); column = (int_of_string column); file;} : t)
	| _ -> raise (Failure (Printf.sprintf "Cannot parse %S into Location" (String.concat " " s)))



let copy ({line; column; file;} : t) : t = 
	({
		line;
		column;
		file;
	} : t)
