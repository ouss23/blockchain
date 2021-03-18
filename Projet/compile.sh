#!/bin/bash

ocamlfind ocamlc -linkpkg -package zarith -I +threads unix.cma threads.cma cryptage.ml transaction.ml block.ml merkle_tree.ml common.ml miner.ml -o miner
ocamlfind ocamlc -linkpkg -package zarith -I +threads unix.cma threads.cma cryptage.ml transaction.ml block.ml merkle_tree.ml common.ml wallet.ml -o wallet
./clear.sh
