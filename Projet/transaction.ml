open Unix
open Encryption

(* type de transactions *)
type transaction = { fromAddress : string; toAddress : string; amount : int; timestamp : int; signature : string }

(* creer une transaction *)
let make_transaction fromAddress_arg toAddress_arg amount_arg =
	{ fromAddress = fromAddress_arg;
	toAddress = toAddress_arg;
	amount = amount_arg;
	timestamp = int_of_float (Unix.time ());
	signature = "" }
	
let transaction_hash tr =
	let m = tr.fromAddress ^ tr.toAddress ^ (string_of_int tr.amount) ^ (string_of_int tr.timestamp) in
	m
	(*string_of_int (Hashtbl.hash m)*)

let signed_transaction from_address_arg to_address_arg amount_arg private_key =
	let tr = make_transaction from_address_arg to_address_arg amount_arg in
	let hsh = transaction_hash tr in
	let sign = sign_transaction_method private_key hsh in
	let tr_s = { tr with signature = sign } in
	tr_s
	
let is_valid tr =
	if (tr.fromAddress = "root") then true
	else
		let hsh = transaction_hash tr in
		verify_sign_method tr.fromAddress hsh tr.signature
	
let header addr =
	if String.length addr > 70 then
		(String.sub addr 66 4)
	else
		addr

let print_transaction tr =
	Format.printf "[%s sent %d to %s at %d]@." (header tr.fromAddress) tr.amount (header tr.toAddress) tr.timestamp

(* afficher une liste de transactions *)
let print_transactions transactions_list = 
    (List.iter print_transaction transactions_list;
    )


