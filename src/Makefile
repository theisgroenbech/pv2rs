
OCB=ocamlbuild
BUILD=$(OCB) -pkg unix -pkg str -I proverif -I rust -I ll

all:
	$(BUILD) main.byte
	$(BUILD) main.native
clean:
	$(OCB) -clean

debug: all
	ocamldebug -I `ocamlfind query ANSITerminal` -I `ocamlfind query str` main.byte
