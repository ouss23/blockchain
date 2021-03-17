#!/bin/bash
ocamlc -I +zarith unix.cma threads.cma transaction.ml block.ml merkle_tree.ml common.ml miner.ml -o miner
ocamlc -I +zarith unix.cma threads.cma transaction.ml block.ml merkle_tree.ml common.ml wallet.ml -o wallet
./clear.sh