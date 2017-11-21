open OUnit2;;
open Conj;;

let test1 test_ctxt = assert_equal true (es_vacio conjunto_vacio);;

let test2 test_ctxt = assert_equal 100 100;;

(* Name the test cases and group them together *)
let suite =
"suite">:::
 ["test1">:: test1;
  "test2">:: test2]
;;

let () =
  run_test_tt_main suite
;;
