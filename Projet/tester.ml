open Unix
open Common
open Transaction
open Merkle_tree
open Encryption

let port = ref 8000

let _ = Arg.parse [] (fun i -> port := int_of_string i) "Wallet"

(* un type qui associe a un nom d'utilisateur une paire de cles *)
type user = { name : string; private_key : string; public_key : string }

(* annuaire d'utilisateurs & cles *)
let users = ref []

(* creer une paire de cle pour un nouveau utilisateur *)
let create_user name =
	let prv = create_private_key () in
	let pub = create_public_key prv in
	let usr = { name=name; private_key=prv; public_key=pub } in
	(* inserer dans l'annuaire *)
	users := usr :: !users;
	(* retourner la paire de cles *)
	usr
	
(* voir si une paire de cles existe pour un nom d'utilisateur donne *)
let user_exists name =
	List.exists (fun x -> x.name = name) !users
	
(* retourner la paire de cles associee a un nom d'utilisateur *)
let find_user name =
	(List.find (fun x -> x.name = name) !users)
	
type test_case = { descr : string; func : (unit -> unit) }

let send_transaction tr =
	let in_chan, out_chan = connect_and_send !port (AddTransaction tr) in
	input_value in_chan

let tr_test1 () =
	let from, _to = (find_user "sender"), (find_user "receiver") in
	let amount = 10 in
	let tr = signed_transaction from.public_key _to.public_key amount from.private_key in
	Format.printf "Sending the following transaction :@.";
	print_transaction tr;
	let answer = send_transaction tr in
	(match answer with
	| Refused m -> Format.printf "Transaction refused : %s@." m
	| _ -> Format.printf "Transaction accepted@.");
	Format.printf "Sending the same transaction again@.";
	let _, _ = connect_and_send !port (AddTransaction tr) in ()
	
let tr_test2 () =
	let from, _to = (find_user "sender"), (find_user "receiver") in
	let amount = 1000 in
	let tr = signed_transaction from.public_key _to.public_key amount from.private_key in
	Format.printf "Sending the following transaction :@.";
	print_transaction tr;
	let answer = send_transaction tr in
	(match answer with
	| Refused m -> Format.printf "Transaction refused : %s@." m
	| _ -> Format.printf "Transaction accepted@.")
	
let tr_test3 () =
	let from, _to = (find_user "sender"), (find_user "receiver") in
	let amount = 10 in
	let tr1 = signed_transaction from.public_key _to.public_key amount from.private_key in
	let tr2 = signed_transaction from.public_key _to.public_key (amount + 10) from.private_key in
	Format.printf "Sending the following transaction :@.";
	print_transaction tr1;
	Format.printf "With the signature of the following transaction :@.";
	print_transaction tr2;
	let answer = send_transaction { tr1 with signature=tr2.signature } in
	(match answer with
	| Refused m -> Format.printf "Transaction refused : %s@." m
	| _ -> Format.printf "Transaction accepted@.")
	
let tr_test4 () =
	let from, _to = (find_user "sender"), (find_user "receiver") in
	let amount = 10 in
	let tr = signed_transaction from.public_key _to.public_key amount _to.private_key in
	Format.printf "Sending the following transaction :@.";
	print_transaction tr;
	Format.printf "After signing it with someone else's private key@.";
	let answer = send_transaction tr in
	(match answer with
	| Refused m -> Format.printf "Transaction refused : %s@." m
	| _ -> Format.printf "Transaction accepted@.")

let () =
	Format.printf "Init ... ";
	let tests = [
		{descr="Resend the same transaction twice"; func=tr_test1};
		{descr="Send an amount higher than our balance"; func=tr_test2};
		{descr="Send a transaction with a wrong signature"; func=tr_test3};
		{descr="Send a transaction signed by someone else"; func=tr_test4}
		] in
	let from, _to = (create_user "sender"), (create_user "receiver") in
	let _, _ = connect_and_send !port (FreeCoins from.public_key) in
	let test_fun x = 
		Format.printf "Testing the following action : %s@." x.descr;
		let _ = read_line() in
		x.func ();
		Format.printf "\n@."
	in
	Format.printf "done@.";
	List.iter test_fun tests;