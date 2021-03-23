open Unix
open Block
open Transaction

module Int =
struct
    type t = int
    let compare = compare
end

module S = Set.Make(Int)

(* Un type qui represente l'etat d'un transaction *)
type transaction_status =
	(* en attente, id du bloc + id de la transaction *)
	| Pending of int * int
	(* validee, id du bloc + id de la transaction + la transaction + la preuve de merkle + le hash de la racine *)
	| Accepted of int * int * transaction * (string list) * string
	(* refusee avec message d'erreur *)
	| Refused of string
	(* non-trouvee *)
	| NotFound

(* Un type qui represente les messages echangees entre les paires et les wallets *)
type message =
	(* connexion avec le numero de port *)
	| Connect of int
	(* recuperation de la liste des paires, les transactions en attentes et la blockchain *)
	| Init of (S.t) * (transaction list) * (block list)
	(* un nouveau bloc mine, broadcaster la blockchain et la transaction de recompense du miner *)
	| MinedBlock of (block list) * transaction
	(* faire une transaction *)
	| AddTransaction of transaction
	(* donner des souts gratuites a une adresse *)
	| FreeCoins of string
	(* demander l'etat de la transaction dans la position (id bloc, id transaction) *)
	| GetTransactionStatus of int * int
	(* retourner le type de la transaction *)
	| TransactionStatus of transaction_status
	(* demander le solde d'une adresse *)
	| GetBalance of string
	(* retourner le solde d'une adresse : adresse + solde + solde en attente *)
	| Balance of string * int * int

let ip = inet_addr_of_string "127.0.0.1"

let send chan v =
	output_value chan v;
	flush chan

let connect_and_send d v =
	let in_chan, out_chan = open_connection (ADDR_INET(ip, d)) in
	send out_chan v;
	in_chan, out_chan
