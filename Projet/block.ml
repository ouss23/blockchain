open Transaction
open Merkle_tree

(* type de blocs *)
type block = { id : int; prev_hash : string; nonce : int; hash_root : string; list_transactions :  transaction list }

let hash_root_of_tr tr_l =
	let mtree = make (to_hash_list tr_l) in
	hash_root mtree

(* creer un bloc a partir d'une liste de transactions *)
let make_block_w_transac id_arg prev_hash_arg list_transactions_arg = 
	{id=id_arg;
	prev_hash=prev_hash_arg;
	hash_root=hash_root_of_tr list_transactions_arg;
	nonce = 0;
	list_transactions=list_transactions_arg}

(* Fonction de verification *)
let check d difficulty =
    (String.sub d 0 difficulty) = String.make difficulty '\000'

(* calculer le hash d'un bloc *)
let get_hash b =
    Digest.string (Marshal.to_string b [])

(* voir si un bloc est mine *)
let check_block b difficulty =
    check (get_hash b) difficulty

let print_block b =
    Format.printf "Bloc : id = %d, hash root = [%s], nonce = %d, transactions count = %d @." b.id b.hash_root b.nonce (List.length b.list_transactions);
	print_transactions b.list_transactions
