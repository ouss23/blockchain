open Transaction

(* type de blocs *)
type block = { id : int; mutable nonce : int; mutable list_transactions :  transaction list}

(* creer un bloc a partir d'une liste de transactions *)
let make_block_w_transac id_arg list_transactions_arg = {id=id_arg; nonce = 0; list_transactions=list_transactions_arg}

(* Fonction de verification *)
let check d difficulty =
 (String.sub d 0 difficulty) = String.make difficulty '\000'

(* miner un bloc *)
let rec puzzle b difficulty =
 let d = Digest.string (Marshal.to_string b []) in 
 if check d difficulty then b
 else
  puzzle { b with nonce = b.nonce + 1 } difficulty
