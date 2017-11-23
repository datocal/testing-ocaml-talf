open OUnit2;;
open Conj;;


let rec print_list printer = function
	[] -> ["<-set"]
	| h::t -> (printer h)::(print_list printer t);;

let rec print_conj printer= function 
	Conjunto([]) -> ""
	| Conjunto(e::l) -> String.concat ", " ((printer e)::(print_list printer l));;

let es_vacio_test test_ctxt = 
	assert_equal 
		~msg: "#1.1 - Check empty set"
		~printer: string_of_bool
		true 
		(es_vacio conjunto_vacio);
	assert_equal 
		~msg: "#1.2 - Check 1 element set"
		~printer: string_of_bool
		false
		(es_vacio (Conjunto([1])));
	assert_equal 
		~msg: "#1.3 - Check some elements set"
		~printer: string_of_bool
		false
		(es_vacio (Conjunto([1;2;3;5])));
;;

let pertenece_test test_ctxt = 
	assert_equal
		~msg: "#2 - Check bellow set"
		~printer: string_of_bool
		true
		(pertenece 1 (Conjunto([1;2;3])));;

let agregar_test test_ctxt = 
	assert_equal
		~msg: "#3 - Check normal add"
		~printer: (print_conj string_of_int)
		~cmp: igual
		(Conjunto([1;2;3]))
		(agregar 1 (Conjunto([2;3])));;

let conjunto_of_list_test test_ctxt = 
	assert_equal
		~msg: "#4 - Check list with repetitions set"
		~printer: (print_conj string_of_int)
		~cmp: igual
		(Conjunto([1;2;3]))
		(conjunto_of_list [1;2;3;3]);;


let suprimir_test test_ctxt = 
	assert_equal
		~msg: "#5 - Check normal supr"
		~printer: (print_conj string_of_int)
		~cmp: igual
		(Conjunto([1;2;4]))
		(suprimir 3 (Conjunto([1;2;3;4])));;

let cardinal_test test_ctxt = 
	assert_equal
		~msg: "#6 - Check cardinal 6"
		~printer: string_of_int
		6
		(cardinal (Conjunto([1;4;7;9;2;3])));;

let union_test test_ctxt = 
	assert_equal
		~msg: "#4 - Check union 6 elements"
		~printer: (print_conj string_of_int)
		~cmp: igual
		(Conjunto([1;4;7;9;2;3]))
		(union (Conjunto([4;9;7])) (Conjunto([1;2;3])));;

let interseccion_test test_ctxt = 
	assert_equal
		~msg: "#4 - Check bellow set"
		~printer: string_of_bool
		true
		(pertenece 1 (Conjunto([1;2;3])));;

let diferencia_test test_ctxt = 
	assert_equal
		~msg: "#4 - Check bellow set"
		~printer: string_of_bool
		true
		(pertenece 1 (Conjunto([1;2;3])));;

let incluido_test test_ctxt = 
	assert_equal
		~msg: "#4 - Check bellow set"
		~printer: string_of_bool
		true
		(pertenece 1 (Conjunto([1;2;3])));;

let igual_test test_ctxt = 
	assert_equal
		~msg: "#4 - Check bellow set"
		~printer: string_of_bool
		true
		(pertenece 1 (Conjunto([1;2;3])));;

let list_of_conjunto_test test_ctxt = 
	assert_equal
		~msg: "#4 - Check bellow set"
		~printer: string_of_bool
		true
		(pertenece 1 (Conjunto([1;2;3])));;

let cartesiano_test test_ctxt = 
	assert_equal
		~msg: "#4 - Check bellow set"
		~printer: string_of_bool
		true
		(pertenece 1 (Conjunto([1;2;3])));;

let cartesiano2_test test_ctxt = 
	assert_equal
		~msg: "#4 - Check bellow set"
		~printer: string_of_bool
		true
		(pertenece 1 (Conjunto([1;2;3])));;

(* Name the test cases and group them together *)
let suite =
"suite" >:::
 ["es_vacio_test"  			>:: es_vacio_test;
  "pertenece_test" 			>:: pertenece_test;
  "agregar_test"   			>:: agregar_test;
  "conjunto_of_list_test"	>:: conjunto_of_list_test;
  "suprimir_test"			>:: suprimir_test;
  "cardinal_test"			>:: cardinal_test;
  "union_test"				>:: union_test;
  "interseccion_test"		>:: interseccion_test;
  "diferencia_test"			>:: diferencia_test;
  "incluido_test"			>:: incluido_test;
  "igual_test"				>:: igual_test;
  "list_of_conjunto_test"	>:: list_of_conjunto_test;
  "cartesiano_test"			>:: cartesiano_test;
  "cartesiano2_test"		>:: cartesiano2_test
 ]
;;


let () =
  run_test_tt_main suite
;;


