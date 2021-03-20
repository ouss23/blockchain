open Unix
open Cryptage

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
	Format.printf "Signature %s@." tr_s.signature;
	tr_s
	
let is_valid tr =
	if (tr.fromAddress = "root") then true
	else
		let hsh = transaction_hash tr in
		verify_sign_method tr.fromAddress hsh tr.signature
	
(*
(*Pour signer une transaction il utilise cryptae.ml*)
let sign_transaction transaction private_key = (
	let public_key = create_public_key private_key in

		if public_key ==  transaction.fromAddress then
			begin
				raise (Failure "you can't sign for another user!")
			end;
			
	let tmp = transaction.fromAddress ^ transaction.toAddress ^ (string_of_int transaction.amount) in
	let signature = sign_transaction_method private_key tmp in
	transaction.signature <- signature;

)

(*Pour verifier que la signature d'un transaction est bonne*)
let isValid transaction = (
	if transaction.fromAddress =  "block chain" then
		begin
			true
		end
	else
		begin
			if transaction.fromAddress = "" then
				begin
				raise (Failure "Not signature")
				end
			else
				begin
				let public_key = transaction.fromAddress in
				let message = transaction.fromAddress ^ transaction.toAddress ^ (string_of_int transaction.amount) in
				let verify_sign = verify_sign_method public_key message transaction.signature in
				verify_sign
				end;
			end;
)
*)
	
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


