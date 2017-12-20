open OUnit2;;
open Auto;;

(* Printers *)
let rec print_list printer = function
	[] -> ["<-set"]
	| h::t -> (printer h)::(print_list printer t);;

let rec print_conj printer= function 
	Conj.Conjunto([]) -> ""
	| Conj.Conjunto(e::l) -> String.concat ", " ((printer e)::(print_list printer l));;

let rec print_estado = function 
	Estado(a) -> a;;


(* Funciones AUXILIARES para obtener datos correctos necesarios para las pruebas *)
let get_automata_correcto = 
	Af
 (Conj.Conjunto [Estado "0"; Estado "1"; Estado "2"; Estado "3"],
  Conj.Conjunto [Terminal "a"; Terminal "b"; Terminal "c"], Estado "0",
  Conj.Conjunto
   [Arco_af (Estado "0", Estado "1", Terminal "a");
    Arco_af (Estado "1", Estado "1", Terminal "b");
    Arco_af (Estado "1", Estado "2", Terminal "a");
    Arco_af (Estado "2", Estado "0", Terminal "");
    Arco_af (Estado "2", Estado "3", Terminal "");
    Arco_af (Estado "2", Estado "3", Terminal "c")],
  Conj.Conjunto [Estado "1"; Estado "3"]);;

 let get_er_correcta = 
 	Auto.Union
 (Auto.Concatenacion
   (Auto.Constante (Auto.Terminal "a"), Auto.Constante (Auto.Terminal "be")),
  Auto.Repeticion (Auto.Constante (Auto.Terminal "ce")));;

 let get_gic_correcta = 
 	Gic (Conj.Conjunto [No_terminal "S"; No_terminal "A"; No_terminal "B"],
		Conj.Conjunto [Terminal "a"; Terminal "b"; Terminal "c"],
		Conj.Conjunto [
		Regla_gic (No_terminal "S", [Terminal "a"; No_terminal "A"]);
		Regla_gic (No_terminal "A",
		[Terminal "a"; Terminal "b"; Terminal "c";
		No_terminal "A"]);
		Regla_gic (No_terminal "A", [Terminal "b"; No_terminal "B"]);
		Regla_gic (No_terminal "B",
		[Terminal "b"; Terminal "c"; No_terminal "B"]);
		Regla_gic (No_terminal "B", [])],
		No_terminal "S");;

let get_ap_correcto = 
	Ap (Conj.Conjunto [Estado "0"; Estado "1"; Estado "2"; Estado "3"],
		Conj.Conjunto [Terminal "a"; Terminal "b"],
		Conj.Conjunto [No_terminal ""; No_terminal "A"],
		Estado "0",
		Conj.Conjunto [Arco_ap (Estado "0", Estado "1", Terminal "a",
					No_terminal "",
					[No_terminal "A"; No_terminal ""]);
				Arco_ap (Estado "1", Estado "1", Terminal "a",
					No_terminal "A",
					[No_terminal "A"; No_terminal "A"]);
				Arco_ap (Estado "1", Estado "2", Terminal "b",
					No_terminal "A",
					[]);
				Arco_ap (Estado "2", Estado "2", Terminal "b",
					No_terminal "A",
					[]);
				Arco_ap (Estado "2", Estado "3", Terminal "",
					No_terminal "",
					[No_terminal ""])],
		No_terminal "",
		Conj.Conjunto [Estado "3"]);;

let get_mt_correcta = 
	Mt (Conj.Conjunto [Estado "0"; Estado "1"; Estado "2"; Estado "3";
		Estado "4"; Estado "5"; Estado "6"; Estado "7"],
	Conj.Conjunto [Terminal "a"; Terminal "b"; Terminal "c"],
	Conj.Conjunto [No_terminal ""; Terminal "a"; Terminal "b"; Terminal "c"],
		Estado "0",
		Conj.Conjunto [Arco_mt (Estado "0", Estado "1",
					No_terminal "", No_terminal "", Derecha);
				Arco_mt (Estado "1", Estado "1",  
					Terminal "a", Terminal "a", Derecha);
				Arco_mt (Estado "1", Estado "1", 
					Terminal "b", Terminal "b", Derecha);
				Arco_mt (Estado "1", Estado "1",
					Terminal "c", Terminal "c", Derecha);
				Arco_mt (Estado "1", Estado "2",
					Terminal "c", Terminal "c", Derecha);
				Arco_mt (Estado "2", Estado "3",
					Terminal "a", Terminal "a", Derecha);
				Arco_mt (Estado "3", Estado "4",
					Terminal "b", Terminal "b", Derecha);
				Arco_mt (Estado "1", Estado "5",
					Terminal "c", Terminal "c", Izquierda);
				Arco_mt (Estado "5", Estado "6",
					Terminal "b", Terminal "b", Izquierda);
				Arco_mt (Estado "6", Estado "7",
					Terminal "a", Terminal "a", Izquierda)],
		No_terminal "",
		Conj.Conjunto [Estado "4"; Estado "7"]);;

(* TESTS DEL MÓDULO AUTO *)
let escaner_af_test test_ctxt = 
	assert_equal
		~msg: "#1.1 - Check accept path"
		true
		(escaner_af [Terminal "a";Terminal "a";Terminal "a"] get_automata_correcto);
	assert_equal
		~msg: "#1.2 - Check dont accept path"
		true
		(escaner_af [Terminal "a";Terminal "a"] get_automata_correcto);;

let af_of_er_test test_ctxt =
	assert_equal
		~msg: "#2 - Check af from er"
		(
			Af
			 (Conj.Conjunto
			   [Estado "0"; Estado "1"; Estado "2"; Estado "3"; Estado "4"; Estado "5";
			    Estado "6"; Estado "7"],
			  Conj.Conjunto [Terminal "a"; Terminal "be"; Terminal "ce"], Estado "0",
			  Conj.Conjunto
			   [Arco_af (Estado "0", Estado "1", Terminal "");
			    Arco_af (Estado "0", Estado "5", Terminal "");
			    Arco_af (Estado "1", Estado "2", Terminal "a");
			    Arco_af (Estado "2", Estado "3", Terminal "");
			    Arco_af (Estado "3", Estado "4", Terminal "be");
			    Arco_af (Estado "5", Estado "6", Terminal "");
			    Arco_af (Estado "6", Estado "7", Terminal "ce");
			    Arco_af (Estado "7", Estado "5", Terminal "")],
			  Conj.Conjunto [Estado "4"; Estado "5"])
		)
		(af_of_er get_er_correcta);;

let avanza_test test_ctxt = 
	assert_equal
		~msg: "#3.1 - Check avanza ignore epsilon arcs"
		~printer: (print_conj print_estado)
		(
			Conj.conjunto_vacio
		)
		(avanza (Terminal "a") (Conj.Conjunto [Estado "2"]) get_automata_correcto);
	assert_equal
		~msg: "#3.2 - Check avanza positive case"
		~printer: (print_conj print_estado)
		(
			Conj.Conjunto [Estado "2"]
		)
		(avanza (Terminal "a") (Conj.Conjunto [Estado "1"]) get_automata_correcto);;

let es_regular_test test_ctxt =
	assert_equal
		~msg: "#4 - Check es_regular positive case"
		~printer: string_of_bool
		(
			true
		)
		(es_regular get_gic_correcta);;

let af_of_gic_test test_ctxt =
	assert_equal
		~msg: "#5 - Check af_of_gic positive case"
		(
		  Af
		   (Conj.Conjunto
		     [Estado "S"; Estado "A"; Estado "B"; Estado "0"; Estado "1"; Estado "2";
		      Estado "3"],
		    Conj.Conjunto [Terminal "a"; Terminal "b"; Terminal "c"], Estado "S",
		    Conj.Conjunto
		     [Arco_af (Estado "S", Estado "A", Terminal "a");
		      Arco_af (Estado "A", Estado "1", Terminal "a");
		      Arco_af (Estado "1", Estado "2", Terminal "b");
		      Arco_af (Estado "2", Estado "A", Terminal "c");
		      Arco_af (Estado "A", Estado "B", Terminal "b");
		      Arco_af (Estado "B", Estado "3", Terminal "b");
		      Arco_af (Estado "3", Estado "B", Terminal "c");
		      Arco_af (Estado "B", Estado "0", Terminal "")],
		    Conj.Conjunto [Estado "0"])
		)
		(af_of_gic get_gic_correcta);;

let gic_of_af_test test_ctxt =
	assert_equal
		~msg: "#6 - Check gic_of_af positive case"
		(
			Gic
			   (Conj.Conjunto
			     [No_terminal "0"; No_terminal "1"; No_terminal "2"; No_terminal "3"],
			    Conj.Conjunto [Terminal "a"; Terminal "b"; Terminal "c"],
			    Conj.Conjunto
			     [Regla_gic (No_terminal "0", [Terminal "a"; No_terminal "1"]);
			      Regla_gic (No_terminal "1", [Terminal "b"; No_terminal "1"]);
			      Regla_gic (No_terminal "1", [Terminal "a"; No_terminal "2"]);
			      Regla_gic (No_terminal "2", [No_terminal "0"]);
			      Regla_gic (No_terminal "2", [No_terminal "3"]);
			      Regla_gic (No_terminal "2", [Terminal "c"; No_terminal "3"]);
			      Regla_gic (No_terminal "1", []); Regla_gic (No_terminal "3", [])],
			    No_terminal "0")
		)
		(gic_of_af get_automata_correcto);;

let escaner_ap_test test_ctxt =
	assert_equal
		~msg: "#7 - Check escaner_ap negative case"
		(
			false
		)
		(escaner_ap ([Terminal "a"; No_terminal "A"; Terminal "a"]) get_ap_correcto);;

let escaner_mt_test test_ctxt =
	assert_equal
		~msg: "#8 - Check escaner_mt negative case"
		(
			false
		)
		(escaner_mt ([Terminal "a"; Terminal "b"; Terminal "a"]) get_mt_correcta)

let scpm_test test_ctxt =
	assert_equal
		~msg: "#9 - Check scpm test"
		(
			[("$", "$0ab$"); ("$", "$"); ("a", "a"); ("b", "b"); ("c", "c");
			 ("0$", "1$"); ("1a", "a1"); ("1b", "b1"); ("1c", "c1"); ("1c", "c2");
			 ("2a", "a3"); ("3b", "b4"); ("a1c", "5ac"); ("b1c", "5bc"); ("c1c", "5cc");
			 ("a5b", "6ab"); ("b5b", "6bb"); ("c5b", "6cb"); ("a6a", "7aa");
			 ("b6a", "7ba"); ("c6a", "7ca"); ("a4a", "4"); ("a4b", "4"); ("a4c", "4");
			 ("b4a", "4"); ("b4b", "4"); ("b4c", "4"); ("c4a", "4"); ("c4b", "4");
			 ("c4c", "4"); ("a7a", "7"); ("a7b", "7"); ("a7c", "7"); ("b7a", "7");
			 ("b7b", "7"); ("b7c", "7"); ("c7a", "7"); ("c7b", "7"); ("c7c", "7");
			 ("a4$", "4$"); ("b4$", "4$"); ("c4$", "4$"); ("a7$", "7$"); ("b7$", "7$");
			 ("c7$", "7$"); ("$4a", "$4"); ("$4b", "$4"); ("$4c", "$4"); ("$7a", "$7");
			 ("$7b", "$7"); ("$7c", "$7"); ("4$$", "$"); ("7$$", "$")]
		)
		(scpm (get_mt_correcta) ([Terminal "a"; Terminal "b"]));;

let epsilon_cierre_test test_ctxt = 
	assert_equal
		~msg: "#10 - Check epsilon_cierre positive"
		~printer: (print_conj print_estado)
		~cmp: Conj.igual
		(
			Conj.Conjunto [Estado "0"; Estado "2"; Estado "3"]
		)
		(epsilon_cierre (Conj.Conjunto [Estado "2"]) get_automata_correcto);;

(* Name the test cases and group them together *)
let suite =
"suite">:::
 ["escaner_af_test"  >:: escaner_af_test;
  "af_of_er_test"    >:: af_of_er_test;
  "avanza_test"      >:: avanza_test;
  "es_regular_test"  >:: es_regular_test;
  "af_of_gic_test"   >:: af_of_gic_test;
  "gic_of_af test"   >:: gic_of_af_test;
  "escaner_ap_test"  >:: escaner_ap_test;
  "escaner_mt test"  >:: escaner_mt_test;
  "scpm test"        >:: scpm_test;
  "epsilon_cierre test" >:: epsilon_cierre_test;]
;;

let () =
  run_test_tt_main suite
;;
