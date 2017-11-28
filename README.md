Instrucciones
-------------

Ejecutar el fichero "runtests" de la raíz del proyecto para analizar todos los tests y la cobertura total
>./runtests

Para lanzar cada test individualmente, dentro de la carpeta test ejecutar
>./test NombrePrueba.ml

Para la ejecución de estos dos scripts es necesario tener instaladas las librerías oUnit y bisect_ppx
Se recomienda instalarlas con opam
>apt install opam

>opam install oUnit

>opam install bisect_ppx

>eval $(opam config env)  //Para actualizar las variables de entorno, puede ser necesario ponerlo en el fichero .profile del home del usuario para no lanzarlo cada vez que iniciemos sesión

Si te da algún problema con la ejecución, comprueba que tienes los dos paquetes instalados, junto con ocamlfind
>opam list

>bisect_ppx                    1.3.1  Code coverage for OCaml

>ocamlfind                     1.7.3  A library manager for OCaml

>ounit                         2.0.6  Unit testing framework loosely based on HUnit. It is similar to JUnit, and other XUnit testing frameworks

y que estás usando ocamlfind de opam
>which ocamlfind

>/home/%USER%/.opam/%VERSION%/bin/ocamlfind
