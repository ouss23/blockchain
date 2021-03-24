#!/bin/bash

ocamlfind ocamlc -linkpkg -package zarith -I +threads unix.cma threads.cma encryption.ml transaction.ml merkle_tree.ml block.ml common.ml miner.ml -o miner
ocamlfind ocamlc -linkpkg -package zarith -I +threads unix.cma threads.cma encryption.ml transaction.ml merkle_tree.ml block.ml common.ml wallet.ml -o wallet
ocamlfind ocamlc -linkpkg -package zarith -I +threads unix.cma threads.cma encryption.ml transaction.ml merkle_tree.ml block.ml common.ml tester.ml -o tester
./clear.sh
