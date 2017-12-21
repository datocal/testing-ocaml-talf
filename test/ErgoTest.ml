(*****************************************************************************
 *
 * ergo.ml   Funciones de conversión desde string o file para los tipos de   
 *           auto.ml.                                                        
 *
 *****************************************************************************)

open Conj;;
open Auto;;
open Ergo;;
open OUnit2;;

let rec generate_string list = function
	 0 -> list
	| n -> generate_string ("a"::list) (n-1);;

(* let assert_raises e k p = 
	try let ex = k p in  true 
	with e -> false;; *)
(*****************************************************************************
 *
 * cadena_of_string : string -> Auto.simbolo list
 * string_of_cadena : Auto.simbolo list -> string
 *
 * Función que dado un string devuelve la lista de símbolos correspondiente, 
 * y viceversa.                                                              
 *
 *****************************************************************************)

let string_of_cadena_test test_ctxt =
	assert_equal 
		~msg: "#1.1 - Check empty cadena"
		""
		(string_of_cadena [Auto.Terminal ""]);
	assert_equal 
		~msg: "#1.2 - Check 1 symbol cadena"
		"c"
		(string_of_cadena [Auto.Terminal "c"]);
	assert_equal 
		~msg: "#1.3 - Check 1 rare symbol cadena"
		"ñá?$~@^"
		(string_of_cadena [Auto.Terminal "ñá?$~@^"]);
	let big_string = (String.concat "" (generate_string [] 1000)) in
		assert_equal 
			~msg: "#1.4 - Check a various symbols cadena"	
			big_string
			(string_of_cadena [Auto.Terminal big_string])
;;

let cadena_of_string_test test_ctxt = 
	assert_equal 
		~msg: "#1.1 - Check empty string"
		[] 
		(cadena_of_string "epsilon");
	assert_equal 
		~msg: "#1.2 - Check 1 character string"
		[Auto.Terminal "c"]
		(cadena_of_string "c");
	assert_equal 
		~msg: "#1.3 - Check 1 character string"
		[Auto.Terminal "ñá?$~@^"]
		(cadena_of_string "ñá?$~@^");
	let big_string = (String.concat "" (generate_string [] 50000)) in
		assert_equal 
			~msg: "#1.4 - Check a various character string"	
			[Auto.Terminal big_string]
			(cadena_of_string big_string)
;;

(*****************************************************************************
 *
 * cadena_of_file : string -> Auto.simbolo list
 * file_of_cadena : Auto.simbolo list -> string -> unit
 *
 * Función que dado un nombre de fichero devuelve la lista de símbolos       
 * correspondiente, y viceversa.                                             
 *
 *****************************************************************************)
 
 (* let cadena_of_file_test test_ctxt =
	assert_equal
		~msg: "#1.1 Check without a file"
		false
		(assert_raises
			(Sys_error ": No such file or directory")
			(cadena_of_file)
			(""));;
	assert_equal
		~msg: "#1.2 Check with a binary file"
		false
		(assert_raises
			(Parsing.Parse_error)
			(cadena_of_file)
			("af_lex.cmi"));
	assert_equal
		~msg: "#1.3 Check with an empty file"
		([])
		(cadena_of_file "empty.txt");
;; *)
 
(*****************************************************************************
 *
 * er_of_string : string -> Auto.er
 * string_of_er : Auto.er -> string
 *
 * Función que dado un string devuelve la expresion regular correspondiente, 
 * y viceversa.                                                              
 *
 *****************************************************************************)
 
  let string_of_er_test test_ctxt =
	assert_equal 
		~msg: "#1.1 - Check empty er"
		"epsilon" 
		(string_of_er (Auto.Constante (Auto.Terminal "")));
	assert_equal 
		~msg: "#1.2 - Check 1 symbol er"
		"c"
		(string_of_er (Auto.Constante (Auto.Terminal "c")));
	let big_string = (String.concat "" (generate_string [] 50000)) in
		assert_equal 
			~msg: "#1.3 - Check a various symbols er"
			big_string
			(string_of_er (Auto.Constante (Auto.Terminal big_string)))
;;

 let er_of_string_test test_ctxt =
	assert_equal 
		~msg: "#1.1 - Check empty string"
		~printer: string_of_er
		(Auto.Vacio)
		(er_of_string "vacio");
	assert_equal 
		~msg: "#1.2 - Check 1 character string"
		~printer: string_of_er
		(Auto.Constante (Auto.Terminal "c"))
		(er_of_string "c");
	let big_string = (String.concat "" (generate_string [] 50000)) in
		assert_equal 
			~msg: "#1.3 - Check a various character string"
			~printer: string_of_er	
			(Auto.Constante (Auto.Terminal big_string))
			(er_of_string big_string)
;;

(*****************************************************************************
 *
 * af_of_string : string -> Auto.af
 * string_of_af : Auto.af -> string
 *
 * Función que dado un string devuelve el autómata finito correspondiente,   
 * y viceversa.                                                              
 *
 *****************************************************************************)
 
 let string_of_af_test test_ctxt =
	let a1 = af_of_string "0 1; a b; 0; 1; 0 1 a; 1 1 a; 1 1 b;" in
		assert_equal 
			~msg: "#1.1 - Check with an Af"
			 "0 1;\na b;\n0;\n1;\n0 1 a;\n1 1 a;\n1 1 b;\n"
			(string_of_af a1)
;;

  let af_of_string_test test_ctxt =
	assert_equal 
		~msg: "#1 - Check normal string"
		~printer: string_of_af
		(Auto.Af (Conjunto [Estado "0"; Estado "1"], Conjunto [Terminal "a"; Terminal "b"],
				Estado "0",
				Conjunto	[Arco_af (Estado "0", Estado "1", Terminal "a");
							Arco_af (Estado "1", Estado "1", Terminal "a");
							Arco_af (Estado "1", Estado "1", Terminal "b")],
				Conjunto [Estado "1"]))
		(af_of_string "0 1; a b; 0; 1; 0 1 a; 1 1 a; 1 1 b;")
;;

(*****************************************************************************
 *
 * gic_of_string : string -> Auto.gic
 * string_of_gic : Auto.gic -> string
 *
 * Función que dado un string devuelve la gramática independiente del        
 * contexto correspondiente, y viceversa.                                    
 *
 *****************************************************************************)
 
 let string_of_gic_test test_ctxt =
	let gic = (gic_of_string "S A B; a b c; S; S -> a A; A -> a b c A | b B; B -> b c B | epsilon;") in
	assert_equal
		~msg: "#1.1 - Check with a GiC"
		("S A B;\na b c;\nS;\nS -> a A;\nA -> a b c A;\nA -> b B;\nB -> b c B;\nB -> epsilon;\n")
		(string_of_gic gic)
;;

		
 let gic_of_string_test test_ctxt =
	assert_equal 
		~msg: "#1.1 - Check with a GiC string"
		~printer: string_of_gic
		(Auto.Gic (Conjunto [No_terminal "S"; No_terminal "A"; No_terminal "B"],
		Conjunto [Terminal "a"; Terminal "b"; Terminal "c"],
		Conjunto
		[Regla_gic (No_terminal "S", [Terminal "a"; No_terminal "A"]);
		Regla_gic
		(No_terminal "A",
		[Terminal "a"; Terminal "b"; Terminal "c"; No_terminal "A"]);
		Regla_gic (No_terminal "A", [Terminal "b"; No_terminal "B"]);
		Regla_gic
		(No_terminal "B", [Terminal "b"; Terminal "c"; No_terminal "B"]);
		Regla_gic (No_terminal "B", [])],
		No_terminal "S"))
		(gic_of_string "S A B; a b c; S; S -> a A; A -> a b c A | b B; B -> b c B | epsilon;");
		
;; 

(*****************************************************************************
 *
 * ap_of_string : string -> Auto.ap
 * string_of_ap : Auto.ap -> string
 *
 * Función que dado un string devuelve el autómata de pila correspondiente,   
 * y viceversa.                                                              
 *
 *****************************************************************************)
 
 let string_of_ap_test test_ctxt= 
	let ap = (ap_of_string "0 1 2 3; a b; zeta A; 0; 3; 0 1 a zeta A zeta; 1 1 a A A A; 1 2 b A epsilon;") in
		assert_equal
			~msg: "#1.1 - Check with an Ap"
			("0 1 2 3;\na b;\nzeta A;\n0;\n3;\n0 1 a zeta A zeta;\n1 1 a A A A;\n1 2 b A epsilon;\n")
			(string_of_ap ap)
;;

let ap_of_string_test test_ctxt=
	assert_equal
		~msg: "#1.1 - Check with an Ap string"
		~printer: string_of_ap
		(Auto.Ap
 (Conjunto [Estado "0"; Estado "1"; Estado "2"; Estado "3"],
  Conjunto [Terminal "a"; Terminal "b"],
  Conjunto [No_terminal ""; No_terminal "A"], Estado "0",
  Conjunto
   [Arco_ap
     (Estado "0", Estado "1", Terminal "a", No_terminal "",
      [No_terminal "A"; No_terminal ""]);
    Arco_ap
     (Estado "1", Estado "1", Terminal "a", No_terminal "A",
      [No_terminal "A"; No_terminal "A"]);
    Arco_ap (Estado "1", Estado "2", Terminal "b", No_terminal "A", [])],
  No_terminal "", Conjunto [Estado "3"]))

	(ap_of_string "0 1 2 3; a b; zeta A; 0; 3; 0 1 a zeta A zeta; 1 1 a A A A; 1 2 b A epsilon;")
;;

(*****************************************************************************
 *
 * mt_of_string : string -> Auto.mt
 * string_of_mt : Auto.mt -> string
 *
 * Función que dado un string devuelve la máquina de Turing correspondiente,   
 * y viceversa.                                                              
 *
 *****************************************************************************)
 
 let string_of_mt_test test_ctxt = 
	let mt = (mt_of_string "0 1 2 3 4 5 6 7; a b c; blanco a b c; 0; 4 7;") in
		assert_equal
			~msg: "#1.1 - Check with a mt"
			("0 1 2 3 4 5 6 7;\na b c;\nblanco a b c;\n0;\n4 7;\n")
			(string_of_mt mt)
;;

let mt_of_string_test test_ctxt =
	assert_equal
		~msg: "#1.1 - Check with a mt string"
		~printer: string_of_mt
		(Mt
   (Conjunto
     [Estado "0"; Estado "1"; Estado "2"; Estado "3"; Estado "4"; Estado "5";
      Estado "6"; Estado "7"],
    Conjunto [Terminal "a"; Terminal "b"; Terminal "c"],
    Conjunto [No_terminal ""; Terminal "a"; Terminal "b"; Terminal "c"],
    Estado "0", Conjunto [], No_terminal "",
    Conjunto [Estado "4"; Estado "7"]))
    (mt_of_string "0 1 2 3 4 5 6 7; a b c; blanco a b c; 0; 4 7;")
;;
			
(* Name the test cases and group them together *)
let suite =
"suite">:::
 ["cadena_of_string_test"  			>:: cadena_of_string_test;
  "string_of_cadena_test" 			>:: string_of_cadena_test;
  "er_of_string_test"				>:: er_of_string_test;
  "string_of_er_test"				>:: string_of_er_test;
  "af_of_string_test"				>:: af_of_string_test;
  "string_of_af_test"				>:: string_of_af_test;
  "gic_of_string_test"				>:: gic_of_string_test;
  "string_of_gic_test"				>:: string_of_gic_test;
  "string_of_ap_test"				>:: string_of_ap_test;
  "ap_of_string_test"				>:: ap_of_string_test;
  "mt_of_string_test"				>:: mt_of_string_test;
  "string_of_mt_test"				>:: string_of_mt_test]
 
;;

let () =
  run_test_tt_main suite
;;
