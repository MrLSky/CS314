open Project2;;

let tokensListFromString (str : string) = Str.split (Str.regexp_string " ") str;;

let buildCNFFromString (str : string) = buildCNF (tokensListFromString str);;

let satisfyFromString (str : string) = satisfy (tokensListFromString str);;

(* (string * bool) list *)
let printSatis (input : (string * bool) list) : unit = 
  let f a = match a with (st, b) -> 
    if b then (
      print_string st;
      print_string " ";
    )
    else 
      print_string "_ ";
  in ignore (List.map f input);
  print_endline ""
;;