open Unix
open Common
open Transaction
open Merkle_tree
open Encryption

let port = ref 8000

let _ = Arg.parse [] (fun i -> port := int_of_string i) "Wallet"

type user = { name : string; private_key : string; public_key : string }

let users = ref []

let create_user name =
	let prv = create_private_key () in
	let pub = create_public_key prv in
	let usr = { name=name; private_key=prv; public_key=pub } in
	users := usr :: !users;
	usr
	
let user_exists name =
	List.exists (fun x -> x.name = name) !users
	
let find_user name =
	(List.find (fun x -> x.name = name) !users)

let () =
    while true do
        Format.printf "> %!";
		let line = read_line() in
        match String.split_on_char ' ' line with
		| "create" :: uname :: _ ->
			begin
				if user_exists uname then
					Format.printf "A user with name %s already exists@." uname
				else
					let usr = create_user uname in
					Format.printf "Created keypair for %s, public key starts with %s@." 
						uname (header usr.public_key)
					
			end
		| "reward" :: uname :: _ ->
			if not (user_exists uname) then
				Format.printf "User %s has no keypair@." uname
			else
			begin
				let usr = find_user uname in
				let in_chan, out_chan = connect_and_send !port (FreeCoins usr.public_key) in
				let answer = input_value in_chan in
				match answer with
				| Balance (id_, confirmed, pending) ->
					Format.printf "Balance of %s is %d, pending balance is %d@." (header id_) confirmed pending
				| _ -> Format.printf "Bad answer@."
			end
        | "send" :: sender :: receiver :: amount :: _ -> 
			begin
				if not (user_exists sender) then
					Format.printf "User %s has no keypair@." sender
				else if not (user_exists receiver) then
					Format.printf "User %s has no keypair@." receiver
				else
				begin
					let rec_kp, sen_kp = (find_user receiver), (find_user sender) in
					let tr = signed_transaction sen_kp.public_key rec_kp.public_key
						(int_of_string amount) sen_kp.private_key in
					let in_chan, out_chan = connect_and_send !port (AddTransaction tr)
					in
					let answer = input_value in_chan in
					match answer with
						| Pending (bl_id, tr_id) -> Format.printf "Transaction is pending, will be added to %d-%d@." bl_id tr_id;
						| Accepted (bl_id, tr_id, tr_, prf, bl_hs) -> 
							Format.printf "Transaction added to %d-%d@." bl_id tr_id;
							Format.printf "Transaction authenticity : %b@." (authenticate (string_hash tr) prf bl_hs);
						| Refused m -> Format.printf "Transaction refused : %s@." m;
						| _ -> Format.printf "Bad answer@.";
				end
			end
		| "balance" :: uname :: _ -> 
			begin
				if not (user_exists uname) then
					Format.printf "User %s has no keypair@." uname
				else
					let id = (find_user uname).public_key in
					let in_chan, out_chan = connect_and_send !port (GetBalance id) in
					let answer = input_value in_chan in
					match answer with
					| Balance (id_, confirmed, pending) ->
						Format.printf "Balance of %s is %d, pending balance is %d@." (header id_) confirmed pending;
					| _ -> Format.printf "Bad answer@.";
			end
		| "follow" :: bid :: tid :: _ ->
			begin
				let in_chan, out_chan = connect_and_send !port
				(GetTransactionStatus (int_of_string bid, int_of_string tid)) in
				let answer = input_value in_chan in
				match answer with
				| Pending (bl_id, tr_id) -> 
					Format.printf "Transaction is pending, will be added to %d-%d@." bl_id tr_id;
				| Accepted (bl_id, tr_id, tr, prf, bl_hs) ->
					let tr_fake = make_transaction "fake toto" "fake titi" 10 in
					Format.printf "Transaction added to %d-%d@." bl_id tr_id;
					print_transaction tr;
					Format.printf "Transaction authenticity : %b@." (authenticate (string_hash tr) prf bl_hs);
					Format.printf "Testing authenticity with fake transaction : %b@." (authenticate (string_hash tr_fake) prf bl_hs);
				| NotFound -> Format.printf "Transaction not found@."
				| _ -> Format.printf "Bad answer@.";
			end
        | "end" :: _ -> exit 0
		| _ -> Format.printf "Wrong command %s@." line;
        (*| m -> let _ = connect_and_send !port (Message m) in ()*)
    done
