open Unix
open Common
open Transaction
open Merkle_tree

let port = ref 8000

let _ = Arg.parse [] (fun i -> port := int_of_string i) "Wallet"

let () =
    while true do
        Format.printf "> %!";
		let line = read_line() in
        match String.split_on_char ' ' line with
        | "send" :: sender :: receiver :: amount :: _ -> 
			begin
				let tr = make_transaction sender receiver (int_of_string amount) in
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
		| "balance" :: id :: _ -> 
			begin
				let in_chan, out_chan = connect_and_send !port (GetBalance id) in
				let answer = input_value in_chan in
				match answer with
				| Balance (id_, amount) -> Format.printf "Balance of %s is %d@." id_ amount;
				| _ -> Format.printf "Bad answer@.";
			end
		| "follow" :: bid :: tid :: _ ->
			begin
				let in_chan, out_chan = connect_and_send !port
				(GetTransactionStatus (int_of_string bid, int_of_string tid)) in
				let answer = input_value in_chan in
				match answer with
				| Pending (bl_id, tr_id) -> Format.printf "Transaction is pending, will be added to %d-%d@." bl_id tr_id;
				| Accepted (bl_id, tr_id, tr, prf, bl_hs) ->
					let tr_fake = make_transaction "fake toto" "fake titi" 10 in
					Format.printf "Transaction added to %d-%d@." bl_id tr_id;
					Format.printf "Transaction authenticity : %b@." (authenticate (string_hash tr) prf bl_hs);
					Format.printf "Testing authenticity with fake transaction : %b@." (authenticate (string_hash tr_fake) prf bl_hs);
				| NotFound -> Format.printf "Transaction not found@."
				| _ -> Format.printf "Bad answer@.";
			end
        | "end" :: _ -> exit 0
		| _ -> Format.printf "Wrong command %s@." line;
        (*| m -> let _ = connect_and_send !port (Message m) in ()*)
    done
