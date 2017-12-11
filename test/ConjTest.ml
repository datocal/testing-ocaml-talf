open OUnit2;;
open Conj;;

let rec string_of_int_pair = function 
	a,b -> String.concat "," [(string_of_int a); (string_of_int b)];;

let rec print_list printer = function
	[] -> ["<-set"]
	| h::t -> (printer h)::(print_list printer t);;

let rec print_conj printer= function 
	Conjunto([]) -> ""
	| Conjunto(e::l) -> String.concat ", " ((printer e)::(print_list printer l));;

let rec generate list= function 
	0 -> list
	|n ->generate (n::list) (n-1);;

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
		(es_vacio (Conjunto(generate [] 50000)))
;;

let pertenece_test test_ctxt = 
	assert_equal
		~msg: "#2.1 - Check bellow set"
		~printer: string_of_bool
		true
		(pertenece 1 (Conjunto([1;2;3])));
	assert_equal
		~msg: "#2.2 - Check not in set"
		~printer: string_of_bool
		false
		(pertenece 4 (Conjunto([1;2;3])));
	assert_equal
		~msg: "#2.3 - Check with empty set"
		~printer: string_of_bool
		false
		(pertenece 1 conjunto_vacio);
	assert_equal
		~msg: "#2.4 - Check with 1 element in set"
		~printer: string_of_bool
		true
		(pertenece 1 (Conjunto([1])));
	assert_equal
		~msg: "#2.5 - Check with 1 char, with the correspondent uppercase in the list"
		~printer: string_of_bool
		false
		(pertenece 'A' (Conjunto(['a'])));
	assert_equal
		~msg: "#2.6 - Check with ñ"
		~printer: string_of_bool
		true
		(pertenece "ñ" (Conjunto(["ñ"])));;


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
		~msg: "#5.1 - Check normal supr"
		~printer: (print_conj string_of_int)
		~cmp: igual
		(Conjunto([1;2;4]))
		(suprimir 3 (Conjunto([1;2;3;4])));
	assert_equal
		~msg: "#5.2 - Check vacío supr"
		~printer: (print_conj string_of_int)
		~cmp: igual
		(Conjunto[])
		(suprimir 3 (Conjunto []));;

let cardinal_test test_ctxt = 
	assert_equal
		~msg: "#6 - Check cardinal 6"
		~printer: string_of_int
		6
		(cardinal (Conjunto([1;4;7;9;2;3])));;

let union_test test_ctxt = 
	assert_equal
		~msg: "#7 - Check union 6 elements"
		~printer: (print_conj string_of_int)
		~cmp: igual
		(Conjunto([1;4;7;9;2;3]))
		(union (Conjunto([4;9;7])) (Conjunto([1;2;3])));;

let interseccion_test test_ctxt = 
	assert_equal
		~msg: "#8 - Check intersection with 1 element"
		~printer: (print_conj string_of_int)
		~cmp: igual
		(Conjunto([1;2;3]))
		(interseccion (Conjunto([1;2;3;4])) (Conjunto([1;2;3;5])));;

let diferencia_test test_ctxt = 
	assert_equal
		~msg: "#9 - Check diferencia minus 1 element"
		~printer: (print_conj string_of_int)
		~cmp: igual
		(Conjunto([1;3;4]))
		(diferencia (Conjunto([1;2;3;4])) (Conjunto([2])));;

let incluido_test test_ctxt = 
	assert_equal
		~msg: "#10 - Check simple include set"
		~printer: string_of_bool
		true
		(incluido (Conjunto([1;3;4])) (Conjunto([1;2;3;4;6])));;

let igual_test test_ctxt = 
	assert_equal
		~msg: "#11 - Check equals altered order"
		~printer:  (print_conj string_of_int)
		~cmp: igual
		(Conjunto([2;1;3]))
		(Conjunto([1;2;3]));;

let list_of_conjunto_test test_ctxt = 
	assert_equal
		~msg: "#12 - Simple list test"
		[1;2;3]
		(list_of_conjunto (Conjunto([1;2;3])));;

let cartesiano_test test_ctxt = 
	assert_equal
		~msg: "#13.1 - Check cartesiano1"
		~printer: (print_conj string_of_int_pair)
		~cmp: igual 
		(Conjunto([1,3;1,4;2,3;2,4]))
		(cartesiano (Conjunto([1;2])) (Conjunto([3;4])));
	assert_equal
		~msg: "#13.2 - Check catesiano vacio 1"
		~printer: (print_conj string_of_int_pair)
		~cmp: igual
		(Conjunto [])
		(cartesiano (Conjunto []) (Conjunto([1;4;6])));
	assert_equal
		~msg: "#13.3 - Check catesiano vacio 2"
		~printer: (print_conj string_of_int_pair)
		~cmp: igual
		(Conjunto [])
		(cartesiano (Conjunto([1;4;6])) (Conjunto []));;

let cartesiano2_test test_ctxt = 
	assert_equal
		~msg: "#14.1 - Check cartesiano2"
		~printer: (print_conj string_of_int_pair)
		~cmp: igual 
		(Conjunto([1,3;1,4;2,3;2,4]))
		(cartesiano2 (Conjunto([1;2])) (Conjunto([3;4])));
	assert_equal
		~msg: "#14.2 - Check catesiano2 vacio 1"
		~printer: (print_conj string_of_int_pair)
		~cmp: igual
		(Conjunto [])
		(cartesiano2 (Conjunto []) (Conjunto([1;4;6])));
	assert_equal
		~msg: "#14.3 - Check catesiano2 vacio 2"
		~printer: (print_conj string_of_int_pair)
		~cmp: igual
		(Conjunto [])
		(cartesiano2 (Conjunto([1;4;6])) (Conjunto []));;


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


