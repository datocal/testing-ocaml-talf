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

let rec print_list printer = function
	[] -> ["<-set"]
	| h::t -> (printer h)::(print_list printer t);;
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
	let big_string = (String.concat "" (generate_string [] 1000)) in
		assert_equal 
			~msg: "#1.3 - Check a various symbols cadena"	
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
	let big_string = (String.concat "" (generate_string [] 50000)) in
		assert_equal 
			~msg: "#1.3 - Check a various character string"	
			[Auto.Terminal big_string]
			(cadena_of_string big_string)
;;

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
 
 (* let gic_of_string_test test_ctxt =
	assert_equal
		~msg: "#1.2 - Check 1 symbol er"
		Auto.Gic 
		(gic_of_string "la gramatica");
;; *)
		
(* Name the test cases and group them together *)
let suite =
"suite">:::
 ["cadena_of_string_test"  			>:: cadena_of_string_test;
  "string_of_cadena_test" 			>:: string_of_cadena_test;
  "er_of_string_test"				>:: er_of_string_test;
  "string_of_er_test"				>:: string_of_er_test;
  "af_of_string_test"				>:: af_of_string_test;
  "string_of_af_test"				>:: string_of_af_test]
 
;;

let () =
  run_test_tt_main suite
;;
