open Unix
open Block
open Transaction

(* Un type qui represente l'etat d'un transaction *)
type transaction_status =
	| Pending of int * int
	| Accepted of int * int (* +proof *)
	| Refused of string
	| NotFound

type message =
	| Connect of int
	| UpdateBlocks of (block list)
	| AddTransaction of transaction
	| GetTransactionStatus of int * int
	| TransactionStatus of transaction_status
	| GetBalance of string
	| Balance of string * int
	(*
	| Message of string
	| ShowPeers
		*)

let ip = inet_addr_of_string "127.0.0.1"

let send chan v =
	output_value chan v;
	flush chan

let connect_and_send d v =
	let in_chan, out_chan = open_connection (ADDR_INET(ip, d)) in
	send out_chan v;
	in_chan, out_chan
