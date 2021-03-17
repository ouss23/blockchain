open Unix

(* type de transactions *)
type transaction = { fromAddress : string; toAddress : string; amount : int; timestamp : int}

(* creer une transaction *)
let make_transaction fromAddress_arg toAddress_arg amount_arg =
	{ fromAddress = fromAddress_arg;
	toAddress = toAddress_arg;
	amount = amount_arg;
	timestamp = int_of_float (Unix.time ())}

let print_transaction tr =
	Format.printf "[%s sent %d to %s at %d]@." tr.fromAddress tr.amount tr.toAddress tr.timestamp

(* afficher une liste de transactions *)
let print_transactions transactions_list = 
    (List.iter print_transaction transactions_list;
    )


