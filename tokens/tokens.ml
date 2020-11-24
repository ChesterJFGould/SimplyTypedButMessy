type t =
	| Lambda of Location.t
	| Identifier of string * Location.t
	| LParen of Location.t
	| RParen of Location.t
	| Dot of Location.t
	| Colon of Location.t
	| Arrow of Location.t



let location (node : t) : Location.t =
	match node with
	| Lambda l -> l
	| Identifier (_, l) -> l
	| LParen l -> l
	| RParen l -> l
	| Dot l -> l
	| Colon l -> l
	| Arrow l -> l



let toString (token : t) : string =
	match token with
	| Lambda l -> "Lambda " ^ (Location.toString l)
	| Identifier (n, l) -> Printf.sprintf "Identifier %s %s" n (Location.toString l)
	| LParen l -> "LParen " ^ (Location.toString l)
	| RParen l -> "RParen " ^ (Location.toString l)
	| Dot l -> "Dot " ^ (Location.toString l)
	| Colon l -> "Colon " ^ (Location.toString l)
	| Arrow l -> "Arrow " ^ (Location.toString l)



let fromString (s : string) : t =
	match String.split_on_char ' ' s with
	| "Lambda"::tl -> Lambda (Location.fromStringList tl)
	| "LParen"::tl -> LParen (Location.fromStringList tl)
	| "RParen"::tl -> RParen (Location.fromStringList tl)
	| "Dot"::tl -> Dot (Location.fromStringList tl)
	| "Colon"::tl -> Colon (Location.fromStringList tl)
	| "Arrow"::tl -> Arrow (Location.fromStringList tl)
	| "Identifier"::n::tl -> Identifier (n, Location.fromStringList tl)
	| _ -> raise (Failure (Printf.sprintf "Cannot parse %S into Token" s))
