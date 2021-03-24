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
let difficulty = 2

(* liste de transactions en attente *)
let pending_transactions = ref []

(* nombre de transactions par bloc *)
let transactions_per_block = 3

(* l'adresse qui recompense les miners *)
let mining_reward_source = "root"

(* la valeur de prev_hash a mettre dans genesis *)
let genesis_prev_hash = "0"

(* un mutex protegeant l'acces a la liste de transactions et de blocs *)
let lock = Mutex.create()

let print_blockchain () =
	List.iter print_block !mined_blocks;
	Format.printf "@."

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
		
(* verifie si la liste l2 contient tous les elements de l1 *)
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
		(* si la taille de la blockchain change, arreter le mining
		car un autre miner a trouve le nonce en premier *)
		if (List.length !mined_blocks = n_bl) then
 			puzzle { b with nonce = b.nonce + 1 } difficulty n_bl
		else
			None
	end
	   
(* Fonction qui mine un block avec la liste des transactions en attente
On met l'adresse du mineur pour qu'il soit recompense *)
let mine_pending_transactions mining_reward_address = (
	Mutex.lock lock;
	let n_bl = List.length !mined_blocks in
	let n_tr = List.length !pending_transactions in
	Mutex.unlock lock;
	
	(* il faut avoir suffisemment de transactions pour miner *)
	if (n_tr < transactions_per_block) then Format.printf "No enough transactions to mine@."
	else
	begin
		(* prendre les N premieres transactions *)
    	let first_transactions = List.init transactions_per_block (fun i -> List.nth !pending_transactions i) in
		Format.printf "Mining the following transactions :@.";
		print_transactions first_transactions;
		(* le nouveau bloc *)
		let current_block = make_block_w_transac (List.length !mined_blocks) (last_bloc_hash ()) first_transactions in
		(* commencer a miner a partir d'une valeur aleatoire, pour ne pas toujours avoir le meme miner qui gagne *)
		let start_from = (Random.int 10000000) in
		Format.printf "Starting at nonce : %d@." start_from;
		let mined_block = puzzle { current_block with nonce = start_from } difficulty n_bl in
		(* on regarde si ce miner est le premier a avoir mine le bloc *)
		match mined_block with
		(* bloc mine par ce miner *)
		| Some b ->
			(Mutex.lock lock;
			if (((List.length !mined_blocks) = n_bl) && (contains_all first_transactions !pending_transactions)) then
			begin
				mined_blocks := !mined_blocks @ [b];
				let reward = make_transaction mining_reward_source mining_reward_address miningReward in
    			pending_transactions := reward :: (exclude_all first_transactions !pending_transactions);
				Format.printf "Block %d mined with nonce %d, broadcasting blockchain...@." b.id b.nonce;
				broadcast (MinedBlock (!mined_blocks, reward));
				Format.printf "Current blockchain :@.";
				print_blockchain ()
			end
			else
				(* bloc mine par un autre miner *)
    			begin Format.printf "Blockchain changed, dropping mined block@." end;
			Mutex.unlock lock;)
		(* bloc mine par un autre miner *)
		| None -> Format.printf "Blockchain changed, mining interrupted@.";
	end
)

(* Retourne un pair (solde, solde en attente 'pending') *)
let get_balance_of_address address blockchain pending_tr = (
	(* solde total *)
	let balance = ref 0 in
	
	(* compter une transaction *)
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
		List.iter from_transac current_block.list_transactions) blockchain;
	(* solde dans les transactions minees *)
	let confirmed = !balance in
	(* prise en compte des transactions en attente *)
	List.iter from_transac pending_tr;
	(* la nouvelle valeur de balance contient le solde en attente *)
	(confirmed, !balance)
)

(* retourner l'indice d'un element e dans la liste l *)
let index_of e l =
	let rec aux e2 l2 i =
		match l2 with
		| [] -> None
		| hd :: tl -> if hd = e2 then Some i else aux e2 tl (i+1)
	in
	aux e l 0

(* retourner la preuve de merkle avec l'indice de la transaction et du bloc *)
let get_transaction_proof bl_id tr_id =
	let mtree = make (to_hash_list (List.nth !mined_blocks bl_id).list_transactions) in
	(* on retourne la preuve + le hash de la racine pour verifier *)
	(proof mtree tr_id), (hash_root mtree)

(* retourner l'etat d'une transaction *)
let get_transaction_status tr =
	match index_of tr !pending_transactions with
	(* transaction en attente *)
	| Some i -> Pending (List.length !mined_blocks, i)
	(* transaction pas en attente, donc soit minee soit non-existante *)
	| None ->
		let rec find_in_blocks bl ind =
			match bl with
			(* transaction pas trouvee dans toute la blockchain *)
			| [] -> NotFound
			| hd :: tl ->
				match index_of tr hd.list_transactions with
				(* transaction minee *)
				| Some i -> 
					let mtree = make (to_hash_list (List.nth !mined_blocks ind).list_transactions) in
					Accepted (ind, i, tr, proof mtree i, hash_root mtree)
				| None -> find_in_blocks tl (ind + 1)
		in
		find_in_blocks !mined_blocks 0
		
(* retourner l'etat de la transaction d'un indice donne (n transaction & n bloc) *)
let get_transaction_status_at block_id tr_id =
	(* rechercher dans la blockchain si l'indice du bloc est < de la taille de la blockchain *)
	if block_id < (List.length !mined_blocks) then
		if tr_id < transactions_per_block then
			let mtree = make (to_hash_list (List.nth !mined_blocks block_id).list_transactions) in
			Accepted (block_id, tr_id,
				List.nth (List.nth !mined_blocks block_id).list_transactions tr_id, proof mtree tr_id, hash_root mtree)
		else
			NotFound
	(* voir dans les transactions en attente si l'indice du bloc est celui du bloc suivant a mine *)
	else if block_id = (List.length !mined_blocks) then 
		if tr_id < (List.length !pending_transactions) then
			Pending (block_id, tr_id)
		else
			NotFound
	else
		NotFound

(* Fonction pour inserer une transaction dans pending_transactions *)
let add_transaction (current_transaction : transaction)= (
	(* on verifie d'abord si l'expediteur a un solde suffisant *)
	let confirmed, pending = get_balance_of_address current_transaction.fromAddress
		!mined_blocks !pending_transactions in
	if (pending < current_transaction.amount) then
		(Format.printf "Refused transaction, pending balance of sender is : %d @." pending;
		Refused "Not enough balance")
	else if (confirmed < current_transaction.amount) then
		(Format.printf "Refused transaction, balance of sender is : %d @." confirmed;
		Refused "Not enough balance")
	(* on verifie si la transaction est correctement signee *)
	else if not (is_valid current_transaction) then
		(Format.printf "Refused transaction, bad signature@.";
		Refused "Bad signature")
	else
	begin
		Mutex.lock lock;
    	pending_transactions := !pending_transactions @ [current_transaction];
		Mutex.unlock lock;
		(* on retourne l'etat de la transaction *)
		get_transaction_status current_transaction
	end
)

(* verifier si tous les soldes (& soldes en attente) des adresses existants sont >= 0 *)
let valid_balances blockchain pending =
	(* parcourir les adresses des transactions en attentes et celles dans les blocs mines *)
	let join_tr = pending :: (List.map (fun b -> b.list_transactions) blockchain) in
	(* on regarde les adresses des expediteurs et beneficiaires *)
	let send_rec_list = List.map (fun tr -> [tr.fromAddress; tr.toAddress]) (List.flatten join_tr) in
	let addresses = List.flatten send_rec_list in
	(* list de toutes les adresses uniques dans blockchain + transactions en attente *)
	let unique_addr = List.fold_right (fun addr l -> 
		if ((List.mem addr l) || (addr = mining_reward_source)) then l else (addr :: l))
		addresses [] in
	Format.printf "Unique addresses :@.";
	List.iter (fun a -> Format.printf "\t%s@." (header a)) unique_addr;
	(* on calcule le solde de toutes les adresses *)
	let balances = List.map (fun addr -> get_balance_of_address addr blockchain pending) unique_addr in
	Format.printf "Balances & pending :@.";
	List.iter (fun (b, p) -> Format.printf "\t%d -> %d@." b p) balances;
	(* on regarde si tous les soldes & soldes en attentes sont positifs *)
	let flat_b = List.fold_right (fun (b, p) l -> (b :: p :: l)) balances [] in
	not (List.exists (fun b -> b < 0) flat_b)

(* mettre a jour la blockchain avec new_chain, prendre la blockchain la plus grande *)
let update_blockchain_with new_chain reward =
	Mutex.lock lock;
	(* on supprime les transactions en attentes si elles ete minees dans la nouvelle blockchain *)
	let new_pending = reward 
			:: (exclude_all (List.flatten (List.map (fun e -> e.list_transactions) new_chain))
				!pending_transactions) in
	(* modifier la blockchain seulement quand la nouvelle version est plus grande *)
	if (List.length !mined_blocks) >= (List.length new_chain) then
		Format.printf "Received blockchain of size %d is ignored because local blockchain is of size %d@."
			(List.length new_chain) (List.length !mined_blocks)
	(* verifier si les nouveaux bloc ont des nonces et des hashes corrects *)
	else if not (check_blocks_authenticity new_chain) then
		Format.printf "Received blockchain is not correct@."
	(* verifier si les nouveaux bloc contiennent des transactions invalides *)
	else if not (valid_balances new_chain new_pending) then
		Format.printf "Received blockchain contains an invalid transaction@."
	(* mettre a jour la blockchain locale *)
	else
		(Format.printf "Updating local blockchain of size %d with a new version of size %d@."
			(List.length !mined_blocks) (List.length new_chain);
		mined_blocks := new_chain;
		pending_transactions := new_pending);
	Mutex.unlock lock
	
(* gonfler le solde d'un utilisateur, pour pouvoir tester les transactions *)
let reward_user id =
	Mutex.lock lock;
	(* creer une transaction de 100 unités depuis "root" *)
	let reward_transaction i =
		let tr = make_transaction mining_reward_source id 100 in
		(* on ajoute 1 secondes pour eviter d'avoir des transactions ayant la meme signature *)
		{ tr with timestamp = (tr.timestamp + i)} in
	(* on cree assez de transactions pour miner un bloc et les valider *)
	pending_transactions := !pending_transactions @
		(List.init transactions_per_block reward_transaction);
	Mutex.unlock lock

(* initialisation du miner *)
let connect_to_peer () =
    match !register with
		(* pas d'autres paires *)
        | None -> 
			(pending_transactions := [];)
		(* recuperer la liste des paires, la blockchain et les transactions en attente depuis un autre paire *)
        | Some r ->
            Format.printf "connecting to %d@." r;
            let in_chan, _ = connect_and_send r (Connect !port) in
			match input_value in_chan with
			| Init (prs, ptr, mbl) -> (peers := prs;
				pending_transactions := ptr; Format.printf "Pending transactions length %d@." (List.length !pending_transactions);
			mined_blocks := mbl)
			| _ -> ()

(* fonction du miner thread *)
let start_miner arg =
	Format.printf "Miner started@.";
	(* reboucler infiniment et voir s'il y a assez de transactions a miner *)
	while true do
    	if (List.length !pending_transactions) >= transactions_per_block then 
		begin 
			(* si ce miner reussi a miner un bloc, la recompense sera donnee a l'adresse "miner_#port" *)
			mine_pending_transactions ("miner_"^(string_of_int !port));
			(* on verifie si la blockchain est correcte apres le mining *)
       		Format.printf "Blockchain authenticity : %b @." (check_blocks_authenticity !mined_blocks)
    	end;
		(* pour ne pas trop occuper le CPU, on attend 1s apres chaque iteration *)
		Unix.sleep 1;
	done

(* fonction du listener thread *)
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
				send out_chan (add_transaction tr);
			| GetBalance id ->
				Format.printf "Received balance check request from wallet@.";
				let confirmed, pending = get_balance_of_address id !mined_blocks !pending_transactions in
				send out_chan (Balance (id, confirmed, pending))
			| GetTransactionStatus (bid, tid) ->
				Format.printf "Received transaction status check request from wallet@.";
				send out_chan (get_transaction_status_at bid tid);
			| FreeCoins id ->
				Format.printf "Giving free coins to %s@." (header id);
				reward_user id;
				let confirmed, pending = get_balance_of_address id !mined_blocks !pending_transactions in
				send out_chan (Balance (id, confirmed, pending))
			| _ -> Format.printf "Bad message type@.";
		end
		else Format.printf "Ignoring duplicate message@."
    done
	
let () =
	(* initialiser le seed du Random avec la valeur de port du miner
	la fonction random est utilisee dans mine_pending_transaction
	pour commencer le mining a partir d'un nonce aleatoire *)
	Random.init !port;
	let th_fun = (fun i -> start_miner 0) in
	(* lancer le miner *)
	let _ = Thread.create th_fun 0 in
	(* lancer le listener *)
	start_listener 1;