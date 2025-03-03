#/bin/sh
cp specParsermly.bkp specParser.mly
# menhir --depend --infer-write-query mock.ml specParser.mly
# ocamlc -i mock.ml > sig.ml
# menhir --infer-read-reply sig.ml specParser.mly
menhir specParser.mly
cp specParser.mly specParsermly.bkp
rm specParser.mly
