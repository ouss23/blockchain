open Unix
open Common
open Block
open Transaction

let port = ref 8000

let register = ref None
let set_register x = register := Some x

let options = [
    "-register", Arg.Int set_register, " Register to a peer";
]

let _ = Arg.parse options (fun i -> port := int_of_string i) "P2P"

(* Adresse IP *)
let addr = ADDR_INET(ip,!port)

(* création de la socket IPv4, TCP *)
let s = socket PF_INET SOCK_STREAM 0

module Int =
struct
    type t = int
    let compare = compare
end

module S = Set.Make(Int)

let peers = ref (S.singleton !port)

(* liste des messages, stockés sous forme de hash *)
let messages = ref S.empty

(* calcule et retourne le hash de m *)
let hash m = Hashtbl.hash m

(* ajoute un message (en hash) m à la liste des messages *)
let add_message m = messages := S.add (hash m) !messages

(* vérifie si un message m est déjà reçu (comparer avec les hash) *)
let already_received m = S.mem (hash m) !messages

let print_peers () =
    Format.printf "Peers : ";
    S.iter (Format.printf "%d ") !peers;
    Format.printf "@."

let connect_to_peer () =
    match !register with
        | None -> ()
        | Some r ->
            Format.printf "connecting to %d@." r;
            let in_chan, _ = connect_and_send r (Connect !port) in
            peers := input_value in_chan

let broadcast v =
    S.iter (fun p ->
        if p <> !port then let _ = connect_and_send p v in ()) !peers
			
(* Montant de la recompense une fois un bloc mine *)
let miningReward = 10

(* Liste des block mines *)
let mined_blocks = ref []

(* difficulte de minning *)
let difficulty = 3

(* liste de transactions en attente de la blockchain *)
let pending_transactions = ref []

(* nombre de transactions par bloc *)
let transactions_per_block = 3

(* l'adresse qui recompense les miners *)
let miningRewardFromAddress = "block chain"

(* la valeur de prev_hash du premier bloc *)
let genesis_prev_hash = "0"

(* verifier si une liste de blocs est correcte
l'enchainement des hashes + les bons nonces *)
let check_blocks_authenticity b_l =
    match b_l with
    (* liste contenant au moins 1 bloc *)
    | hd :: tl -> 
        (* pour le genesis, on verifie juste qu'on a le bon nonce *)
        if (check_block hd difficulty) = false then
            false
        (* pour les blocs restants, on verifie l'enchainement des hashes *)
        else
            let rec aux b_l2 prev_hash =
                match b_l2 with
                (* verifier le nonce & l'enchainement de hashes *)
                | a :: q -> if ((check_block a difficulty) = false) || ((prev_hash = a.prev_hash) = false) then false
                            else aux q (get_hash a)
                (* fin de la liste *)
                | _ -> true
            in
            aux tl (get_hash hd)
    (* liste vide, on retourne True *)
    | _ -> true

let last_bloc_hash () =
    if (List.length !mined_blocks) = 0 then
        genesis_prev_hash
    else
        get_hash (List.hd (List.rev !mined_blocks))
		
(* Fonction qui mine un block avec la liste des transactions en attente
On met l'adresse du mineur pour qu'il soit recompense *)
let minePendingTransactions miningRewardAdress = (
	Format.printf "Mining the following transactions :@.";
	print_transactions !pending_transactions;
    let first_transactions = List.init transactions_per_block (fun i -> List.nth !pending_transactions i) in
    let remaining_transactions = List.init ((List.length !pending_transactions) - transactions_per_block)
                                            (fun i -> List.nth !pending_transactions (i + transactions_per_block)) in
	let current_block = make_block_w_transac (List.length !mined_blocks) (last_bloc_hash ()) first_transactions in
    let mined_block = puzzle current_block difficulty in
    mined_blocks := !mined_blocks @ [mined_block];
    pending_transactions := (make_transaction miningRewardFromAddress miningRewardAdress miningReward) :: remaining_transactions;
    
    Format.printf "Block %d mined with nonce %d@." mined_block.id mined_block.nonce;

    pending_transactions := List.init 1 (fun i -> make_transaction miningRewardFromAddress miningRewardAdress miningReward);
)

(* Fonction qui dit l'etat du compte d'une adresse dans une blockchain *)
let getBalanceOfAddress address = (
  let balance = ref 0 in
  List.iter (fun current_block ->
    (* Format.printf "Parse list of block : %d with balance %d @." current_block.id !balance; *)
    List.iter (fun transac ->
        (* Format.printf "fromAddress %s toAddress %s amount %d@." transac.fromAddress transac.toAddress transac.amount; *)

        if address = transac.fromAddress then
        begin
            balance := !balance - transac.amount
        end;

        if address = transac.toAddress then
        begin
            balance := !balance + transac.amount
        end;
    ) current_block.list_transactions
  ) !mined_blocks;
  !balance
)

let index_of e l =
	let rec aux e2 l2 i =
		match l2 with
		| [] -> None
		| hd :: tl -> if hd = e2 then Some i else aux e2 tl (i+1)
	in
	aux e l 0
		

let get_transaction_status tr =
	match index_of tr !pending_transactions with
	| Some i -> Pending (List.length !mined_blocks, i)
	| None ->
		let rec find_in_blocks bl ind =
			match bl with
			| [] -> NotFound
			| hd :: tl ->
				match index_of tr hd.list_transactions with
				| Some i -> Accepted (ind, i)
				| None -> find_in_blocks tl (ind + 1)
		in
		find_in_blocks !mined_blocks 0
		
let get_transaction_status_at block_id tr_id =
	if block_id < (List.length !mined_blocks) then
		if tr_id < transactions_per_block then
			Accepted (block_id, tr_id)
		else
			NotFound
	else if block_id = (List.length !mined_blocks) then 
		if tr_id < (List.length !pending_transactions) then
			Pending (block_id, tr_id)
		else
			NotFound
	else
		NotFound

(* Fonction pour pusher transaction dans pending_transactions *)
let addTransaction (current_transaction : transaction)= (
	if ((getBalanceOfAddress current_transaction.fromAddress) < current_transaction.amount) then
		(Format.printf "Refused transaction, balance of sender is : %d @." (getBalanceOfAddress current_transaction.fromAddress);
		Refused "Not enough balance")
	else
	begin
    	pending_transactions := !pending_transactions @ [current_transaction];
    	if (List.length !pending_transactions) >= transactions_per_block then 
		begin 
			minePendingTransactions "miner";
       		Format.printf "Blockchain authenticity : %b @." (check_blocks_authenticity !mined_blocks)
    	end;
		get_transaction_status current_transaction
	end
)

let () =
	pending_transactions := [make_transaction "root" "toto" 100;
		make_transaction "root" "toto" 100;
		make_transaction "root" "toto" 100];
		
	minePendingTransactions "miner";

    setsockopt s SO_REUSEADDR true;
    bind s addr;
    listen s 5;

    connect_to_peer ();

    while true do
        let sc, _ = accept s in
        let in_chan = in_channel_of_descr sc in
        let out_chan = out_channel_of_descr sc in
        let m = input_value in_chan in
        match m with
        | Connect p ->
            Format.printf "Peer %d is connecting to P2P@." p;
            peers := S.add p !peers;
            send out_chan !peers;
        | UpdateBlocks bl -> List.iter print_block bl
		| AddTransaction tr -> 
			Format.printf "Received transaction request from wallet@.";
			send out_chan (addTransaction tr);
		| GetBalance id ->
			Format.printf "Received balance check request from wallet@.";
			send out_chan (Balance (id, getBalanceOfAddress id))
		| GetTransactionStatus (bid, tid) ->
			Format.printf "Received transaction status check request from wallet@.";
			send out_chan (get_transaction_status_at bid tid);
		| _ -> Format.printf "Bad message type@.";
    done
