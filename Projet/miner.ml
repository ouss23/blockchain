open Unix
open Common
open Block
open Transaction
open Mutex
open Merkle_tree
open Random

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

(*module Int =
struct
    type t = int
    let compare = compare
end

module S = Set.Make(Int)*)

let peers = ref (Common.S.singleton !port)

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

let broadcast v =
	add_message v;
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
let miningRewardFromAddress = "root"

(* la valeur de prev_hash du premier bloc *)
let genesis_prev_hash = "0"

(* un mutex protegeant l'acces a la liste de transactions et de blocs *)
let lock = Mutex.create()

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

(* retourne le hash du dernier block mine *)
let last_bloc_hash () =
    if (List.length !mined_blocks) = 0 then
        genesis_prev_hash
    else
        get_hash (List.hd (List.rev !mined_blocks))
		
(* verifie si la liste 2 contient tous les elements de l1 *)
let contains_all l1 l2 =
	List.for_all (fun x -> List.mem x l2) l1
	
(* une reimplementation de List.filter_map qui n'est pas dispo sur ocaml < 4.08 *)
let rec filter_map f l =
	match l with
	| [] -> []
	| hd :: tl -> 
		(match (f hd) with
		| Some e -> (e :: (filter_map f tl))
		| None -> filter_map f tl)
	
(* exclure de l2 tous les elements de l1, et retourner l2 *)
let exclude_all l1 l2 =
	filter_map (fun x -> if List.mem x l1 then None else Some x) l2	

(* miner un bloc *)
let rec puzzle b difficulty n_bl =
	if check_block b difficulty then Some b
	else
	begin
		if (List.length !mined_blocks = n_bl) then
 			puzzle { b with nonce = b.nonce + 1 } difficulty n_bl
		else
			None
	end
	   
(* Fonction qui mine un block avec la liste des transactions en attente
On met l'adresse du mineur pour qu'il soit recompense *)
let mine_pending_transactions miningRewardAdress = (
	Mutex.lock lock;
	let n_bl = List.length !mined_blocks in
	let n_tr = List.length !pending_transactions in
	Mutex.unlock lock;
	if (n_tr < transactions_per_block) then Format.printf "No enough transactions to mine@."
	else
	begin
    	let first_transactions = List.init transactions_per_block (fun i -> List.nth !pending_transactions i) in
		Format.printf "Mining the following transactions :@.";
		print_transactions first_transactions;
		(*let remaining_transactions = List.init ((List.length !pending_transactions) - transactions_per_block)
    	                 (fun i -> List.nth !pending_transactions (i + transactions_per_block)) in*)
		let current_block = make_block_w_transac (List.length !mined_blocks) (last_bloc_hash ()) first_transactions in
		(* commencer a miner a partir d'une valeur aleatoire, pour ne pas toujours avoir le meme miner qui gagne *)
		let start_from = (Random.int 10000000) in
		Format.printf "Start nonce : %d@." start_from;
		let mined_block = puzzle { current_block with nonce = start_from } difficulty n_bl in
		match mined_block with
		| Some b ->
			(Mutex.lock lock;
			if (((List.length !mined_blocks) = n_bl) && (contains_all first_transactions !pending_transactions)) then
			begin
				mined_blocks := !mined_blocks @ [b];
				let reward = make_transaction miningRewardFromAddress miningRewardAdress miningReward in
    			pending_transactions := reward :: (exclude_all first_transactions !pending_transactions);
				Format.printf "Block %d mined with nonce %d, broadcasting blockchain...@." b.id b.nonce;
				broadcast (MinedBlock (!mined_blocks, reward))
			end
			else
    			begin Format.printf "Blockchain changed, dropping mined block@." end;
			Mutex.unlock lock;)
		| None -> Format.printf "Blockchain changed, mining interrupted@.";
	end
)

(* Retourne un pair (balance, balance with pending) *)
let get_balance_of_address address = (
  let balance = ref 0 in
  let from_transac transac = (
      if address = transac.fromAddress then
      begin
          balance := !balance - transac.amount
      end;

      if address = transac.toAddress then
      begin
          balance := !balance + transac.amount
      end;
  ) in
  List.iter (fun current_block ->
    (* Format.printf "Parse list of block : %d with balance %d @." current_block.id !balance; *)
    List.iter from_transac current_block.list_transactions
  ) !mined_blocks;
  let confirmed = !balance in
  List.iter from_transac !pending_transactions;
  (confirmed, !balance)
)

let index_of e l =
	let rec aux e2 l2 i =
		match l2 with
		| [] -> None
		| hd :: tl -> if hd = e2 then Some i else aux e2 tl (i+1)
	in
	aux e l 0
		
let get_transaction_proof bl_id tr_id =
	let mtree = make (to_hash_list (List.nth !mined_blocks bl_id).list_transactions) in
	(proof mtree tr_id), (hash_root mtree)

let get_transaction_status tr =
	match index_of tr !pending_transactions with
	| Some i -> Pending (List.length !mined_blocks, i)
	| None ->
		let rec find_in_blocks bl ind =
			match bl with
			| [] -> NotFound
			| hd :: tl ->
				match index_of tr hd.list_transactions with
				| Some i -> 
					let mtree = make (to_hash_list (List.nth !mined_blocks ind).list_transactions) in
					Accepted (ind, i, tr, proof mtree i, hash_root mtree)
				| None -> find_in_blocks tl (ind + 1)
		in
		find_in_blocks !mined_blocks 0
		
let get_transaction_status_at block_id tr_id =
	if block_id < (List.length !mined_blocks) then
		if tr_id < transactions_per_block then
			let mtree = make (to_hash_list (List.nth !mined_blocks block_id).list_transactions) in
			Accepted (block_id, tr_id,
				List.nth (List.nth !mined_blocks block_id).list_transactions tr_id, proof mtree tr_id, hash_root mtree)
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
	let confirmed, pending = get_balance_of_address current_transaction.fromAddress in
	if (pending < current_transaction.amount) then
		(Format.printf "Refused transaction, pending balance of sender is : %d @." pending;
		Refused "Not enough balance")
	else if (confirmed < current_transaction.amount) then
		(Format.printf "Refused transaction, balance of sender is : %d @." confirmed;
		Refused "Not enough balance")
	else if not (is_valid current_transaction) then
		(Format.printf "Refused transaction, bad signature@.";
		Refused "Bad signature")
	else
	begin
		Mutex.lock lock;
    	pending_transactions := !pending_transactions @ [current_transaction];
		Mutex.unlock lock;
		get_transaction_status current_transaction
	end
)

let update_blockchain_with new_chain reward =
	Mutex.lock lock;
	if (List.length !mined_blocks) >= (List.length new_chain) then
		Format.printf "Received blockchain of size %d is ignored because local blockchain is of size %d@."
		(List.length new_chain) (List.length !mined_blocks)
	else if not (check_blocks_authenticity new_chain) then
		Format.printf "Received blockchain is not correct@."
	else
		(Format.printf "Updating local blockchain of size %d with a new version of size %d@."
		(List.length !mined_blocks) (List.length new_chain);
		mined_blocks := new_chain;
		pending_transactions := reward 
			:: (exclude_all (List.flatten (List.map (fun e -> e.list_transactions) !mined_blocks))
				!pending_transactions));
	Mutex.unlock lock
	
let reward_user id =
	Mutex.lock lock;
	pending_transactions := !pending_transactions @
		(List.init transactions_per_block (fun x -> make_transaction miningRewardFromAddress id 100));
	Mutex.unlock lock

let connect_to_peer () =
    match !register with
        | None -> 
			(pending_transactions := [];(*[make_transaction "root" "toto" 100;
			make_transaction "root" "toto" 100;
			make_transaction "root" "toto" 100];*))
        | Some r ->
            Format.printf "connecting to %d@." r;
            let in_chan, _ = connect_and_send r (Connect !port) in
			match input_value in_chan with
			| Init (prs, ptr, mbl) -> (peers := prs;
				pending_transactions := ptr; Format.printf "pending transactions length %d@." (List.length !pending_transactions);
			mined_blocks := mbl)
			| _ -> ()

let start_miner arg =
	Format.printf "Miner started@.";
	while true do
    	if (List.length !pending_transactions) >= transactions_per_block then 
		begin 
			mine_pending_transactions "miner";
       		Format.printf "Blockchain authenticity : %b @." (check_blocks_authenticity !mined_blocks)
    	end;
		Unix.sleep 1;
	done

let start_listener arg =
    setsockopt s SO_REUSEADDR true;
    bind s addr;
    listen s 5;

    connect_to_peer ();

    while true do
        let sc, _ = accept s in
        let in_chan = in_channel_of_descr sc in
        let out_chan = out_channel_of_descr sc in
        let m = input_value in_chan in
		if not (already_received m) then
		begin
        	match m with
        	| Connect p ->
        	    Format.printf "Peer %d is connecting to P2P@." p;
        	    peers := S.add p !peers;
        	    send out_chan (Init (!peers, !pending_transactions, !mined_blocks));
        	| MinedBlock (bl, rw) -> 
				add_message m;
				update_blockchain_with bl rw;
			| AddTransaction tr -> 
				add_message m;
				Format.printf "Received transaction request from wallet@.";
				broadcast m;
				send out_chan (addTransaction tr);
			| GetBalance id ->
				Format.printf "Received balance check request from wallet@.";
				let confirmed, pending = get_balance_of_address id in
				send out_chan (Balance (id, confirmed, pending))
			| GetTransactionStatus (bid, tid) ->
				Format.printf "Received transaction status check request from wallet@.";
				send out_chan (get_transaction_status_at bid tid);
			| FreeCoins id ->
				Format.printf "Giving free coins to %s@." (header id);
				reward_user id;
				let confirmed, pending = get_balance_of_address id in
				send out_chan (Balance (id, confirmed, pending))
			| _ -> Format.printf "Bad message type@.";
		end
    done
	

let () =
	Random.init !port;
	let th_fun = (fun i -> start_miner 0) in
	let _ = Thread.create th_fun 0 in
	start_listener 1;