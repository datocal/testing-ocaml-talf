exception Not_Mocked of string;;

type 'a conjunto = Conjunto of 'a list;;

let conjunto_vacio = (Conjunto []);;

let es_vacio= function
	(Conjunto []) -> true
 	|_-> raise (Not_Mocked "Es vacío");;

let pertenece s ns = match s,ns with 
	| _ -> raise (Not_Mocked "Pertenece ");;


let agregar a conj = match a,conj with
   _ -> raise (Not_Mocked "Agregar");;
(*****************************************************************************)

let rec conjunto_of_list = function
   _ -> raise (Not_Mocked "Conj_of_list");;

(*****************************************************************************)

let rec suprimir a conj = match a,conj with
     _ -> raise (Not_Mocked "Suprimir");;

(*****************************************************************************)

let cardinal= function
	_ -> raise (Not_Mocked "Cardinal");;

(*****************************************************************************)

let union a b = match a,b with
	_-> raise (Not_Mocked "Union");;

(*****************************************************************************)

let rec interseccion a b = match a,b with
    _-> raise (Not_Mocked "IntersecciÃ³n");;

(*****************************************************************************)

let rec diferencia a b = match a,b with
    _-> raise (Not_Mocked "Diferencia");;

(*****************************************************************************)

let rec incluido a b = match a,b with
    _-> raise (Not_Mocked "Incluido");;
(*****************************************************************************)

let rec igual a b = match a,b with
    _-> raise (Not_Mocked "Igual");;

(*****************************************************************************)

let list_of_conjunto= function 
	_-> raise (Not_Mocked "list_of_conjunto");;

(*****************************************************************************)


let cartesiano a b = match (a,b) with
  _-> raise (Not_Mocked "cartesiano");;

let cartesiano2 a b = match (a,b) with
  _-> raise (Not_Mocked "cartesiano");;