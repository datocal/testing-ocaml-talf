exception Not_Mocked of string;;

type 'a conjunto = Conjunto of 'a list;;

let conjunto_vacio = (Conjunto []);;

type simbolo =
     Terminal of string 
   | No_terminal of string;;

(*****************************************************************************)

type er =
     Vacio
   | Constante of simbolo
   | Union of (er * er)
   | Concatenacion of (er * er)
   | Repeticion of er;;

(*****************************************************************************)

type estado =
   Estado of string;;

type arco_af =
   Arco_af of (estado * estado * simbolo);;

type af =
   Af of (estado conjunto *
          simbolo conjunto * 
          estado *
          arco_af conjunto * 
          estado conjunto);;

(*****************************************************************************)

type regla_gic =
   Regla_gic of (simbolo * simbolo list);;

type gic =
   Gic of (simbolo conjunto * 
           simbolo conjunto *
           regla_gic conjunto * 
           simbolo);;

(*****************************************************************************)

type arco_ap =
   Arco_ap of (estado * estado * simbolo * simbolo * simbolo list);;

type ap = 
   Ap of (estado conjunto * simbolo conjunto * simbolo conjunto *
          estado * arco_ap conjunto * simbolo * estado conjunto);;

(*****************************************************************************)

type movimiento_mt =
     Izquierda
   | Derecha;;

type arco_mt =
   Arco_mt of (estado * estado * simbolo * simbolo * movimiento_mt);;

type mt =
   Mt of (estado conjunto * simbolo conjunto * simbolo conjunto *
          estado * arco_mt conjunto * simbolo * estado conjunto);;



let es_vacio= function
	(Conjunto []) -> true
 	|_-> raise (Not_Mocked "Es vacío");;

let pertenece s, ns=  match s,ns with 
   (Estado "0"), (Conjunto [Estado "0"]) -> true
  | (Estado "1"), (Conjunto [Estado "0"]) -> false    
  
	| _ -> raise (Not_Mocked "Pertenece ");;


let agregar a conj = match a,conj with
   (Estado "1"), conjunto_vacio -> (Conjunto [Estado "1"])
   |_ -> raise (Not_Mocked "Agregar");;
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