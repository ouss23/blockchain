open Unix

(* type de transactions *)
type transaction = { fromAddress : string; toAddress : string; amount : int; timestamp : int; mutable signature : string }

(* creer une transaction *)
let make_transaction fromAddress_arg toAddress_arg amount_arg =
	{ fromAddress = fromAddress_arg;
	toAddress = toAddress_arg;
	amount = amount_arg;
	timestamp = int_of_float (Unix.time ());
	signature = ""}
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
	

let print_transaction tr =
	Format.printf "[%s sent %d to %s at %d]@." tr.fromAddress tr.amount tr.toAddress tr.timestamp

(* afficher une liste de transactions *)
let print_transactions transactions_list = 
    (List.iter print_transaction transactions_list;
    )


