 #!/bin/bash
ocamlbuild -quiet -use-menhir -tag thread -use-ocamlfind -pkg z3 \
                                                        -I maps \
                                                        -I utils \
                                                        -I speclang \
                                                        -I specparser \
                                                        -I sigmabuilder \
                                                        -I progresslang \
                                                        -I typechecking \
                                                        -I vcencode  \
                                                        -I synlang \
                                                        -I synthesis main/prudent.native
