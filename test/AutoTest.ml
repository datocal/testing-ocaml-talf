open OUnit2;;
open Conj;;

let es_vacio_test test_ctxt = 
	assert_equal 
		~msg: "#1 - Check empty set"
		~printer: string_of_bool
		true 
		(es_vacio conjunto_vacio);;

let pertenece_test test_ctxt = 
	todo "pertenece not implemented";
	assert_equal
		~msg: "#2 - Check bellow set"
		~printer: string_of_bool
		false 
		true;;

(* Name the test cases and group them together *)
let suite =
"suite">:::
 ["es_vacio_test"  >:: es_vacio_test;
  "pertenece_test" >:: pertenece_test]
;;

let () =
  run_test_tt_main suite
;;
