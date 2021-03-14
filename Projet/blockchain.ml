open Transaction
open Block

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
    let first_transactions = List.init transactions_per_block (fun i -> List.nth !pending_transactions i) in
    let remaining_transactions = List.init ((List.length !pending_transactions) - transactions_per_block)
                                            (fun i -> List.nth !pending_transactions (i + transactions_per_block)) in
    let current_block = make_block_w_transac (List.length !mined_blocks) (last_bloc_hash ()) first_transactions in
    let mined_block = puzzle current_block difficulty in
    mined_blocks := !mined_blocks @ [mined_block];
    pending_transactions := (make_transaction miningRewardFromAddress miningRewardAdress miningReward) :: remaining_transactions;
    
    Format.printf "Block %d mined with nonce %d@." mined_block.id mined_block.nonce;

    pending_transactions := List.init 1 (fun i -> make_transaction miningRewardFromAddress miningRewardAdress miningReward);
    print_transactions !pending_transactions;
)

(* Fonction pour pusher transaction dans pending_transactions *)
let addTransaction (current_transaction : transaction)= (
    pending_transactions := current_transaction :: !pending_transactions;
    if (List.length !pending_transactions) >= transactions_per_block then
    begin
        minePendingTransactions "miner";
        Format.printf "Mined blocks authenticity : %b @." (check_blocks_authenticity !mined_blocks)
    end
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

let () =
    addTransaction (make_transaction "from" "to" 10);
    addTransaction (make_transaction "from" "to" 10);
    addTransaction (make_transaction "from" "to" 10);
    addTransaction (make_transaction "from" "to" 10);
    addTransaction (make_transaction "from" "to" 10);
    Format.printf "Balance of to %d @." (getBalanceOfAddress "to");
    Format.printf "Balance of from %d @." (getBalanceOfAddress "from");
    Format.printf "Balance of miner %d @." (getBalanceOfAddress "miner");
    List.iter (fun block -> print_block block) !mined_blocks;
