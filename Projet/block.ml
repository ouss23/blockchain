open Transaction

(* type de blocs *)
type block = { id : int; prev_hash : string; nonce : int; list_transactions :  transaction list }

(* creer un bloc a partir d'une liste de transactions *)
let make_block_w_transac id_arg prev_hash_arg list_transactions_arg = {id=id_arg; prev_hash=prev_hash_arg; nonce = 0; list_transactions=list_transactions_arg}

(* Fonction de verification *)
let check d difficulty =
    (String.sub d 0 difficulty) = String.make difficulty '\000'

(* calculer le hash d'un bloc *)
let get_hash b =
    Digest.string (Marshal.to_string b [])

let check_block b difficulty =
    check (get_hash b) difficulty

let print_block b =
    Format.printf "Bloc : id = %d, prev_hash = [%s], nonce = %d, transactions count = %d @." b.id b.prev_hash b.nonce (List.length b.list_transactions);
