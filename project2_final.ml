
let validVarNames = ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j";"k";"l";"m";"n";"o";"p";"q";"r";"s";"t";"u";"v";"w";"x";"y";"z"];;

let partition (input: string list) (bound : string) : string list list =
	let rec aux acc current = function
		| [] ->		
			if current = [] then List.rev acc
			else List.rev (current :: acc)
		| hd :: tl when hd = bound ->
			aux (current :: acc) [] tl
		| hd :: tl ->
			aux acc (hd :: current) tl
	in 
	List.rev (aux [][] (List.rev input))
;;

let buildCNF (input : string list) : (string * string) list list =
	let parStr = partition input "AND" in
	List.map (fun clause ->
		let rec con acc = function
			| [] -> List.rev acc 
        		| "(" :: rest | ")" :: rest -> con acc rest
    	  		| "OR" :: rest -> con acc rest
        		| "AND" :: rest -> con acc rest
        		| "NOT" :: var :: rest -> con ((var, "NOT") :: acc) rest
        		| var :: rest -> con ((var, "") :: acc) rest
      		in
      		con [] clause
    	) parStr 
;;

let getVariables (input : string list) : string list = 
	let findVar var = List.mem var validVarNames in

	let rec collect_vars acc = function
		| [] -> List.rev acc
		| hd :: tl ->
			if findVar hd && not (List.mem hd acc) then
				collect_vars (hd :: acc) tl
			else
				collect_vars acc tl
	in
	collect_vars [] input
;;

let rec generateDefaultAssignments (varList : string list) : (string * bool) list = 
	match varList with
	| [] -> []
	| hd :: tl -> (hd, false) :: generateDefaultAssignments tl
;;

let rec generateNextAssignments (assignList : (string * bool) list) : (string * bool) list * bool =
  let rec helper assignList carry =
    match assignList with
    | [] -> [], carry
    | (var, false) :: tl -> 
        (var, true) :: tl, false
    | (var, true) :: tl -> 
        let uT, new_carry = helper tl true in
        (var, false) :: uT, new_carry
  in
  let reversed_assignList = List.rev assignList in
  let result, carry = helper reversed_assignList false in
  List.rev result, carry
;;

let rec lookupVar (assignList : (string * bool) list) (str : string) : bool = 
	match assignList with
	| [] -> failwith "Var Not Found"
	| (var, value) :: rest ->
		if var = str then value
		else
			lookupVar rest str
;;

let evaluateCNF (t : (string * string) list list) (assignList : (string * bool) list) : bool = 
	let eval_clause clause =
		List.exists (fun (var, negation) ->
			let value = lookupVar assignList var in
			if negation = "NOT" then not value
			else
				value
		) clause
	in
	List.for_all eval_clause t
;;

let satisfy (input : string list) : (string * bool) list =
	let vars = getVariables input in
	let default_ass = generateDefaultAssignments vars in
	let bCNF = buildCNF input in
	let rec find_SAT assignment =
		if evaluateCNF bCNF assignment then
			assignment
		else
			let next_ass, carry = generateNextAssignments assignment in
			if carry then
				[("Error", true)]
			else
				find_SAT next_ass
	in
	find_SAT default_ass
;;