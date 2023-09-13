for sample in test/samples/*.mc; do dune exec test/parser_test.exe -- $sample; done

