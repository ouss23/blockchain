#!/bin/bash
ocamlc unix.cma transaction.ml block.ml common.ml miner.ml -o miner
ocamlc unix.cma transaction.ml block.ml common.ml wallet.ml -o wallet