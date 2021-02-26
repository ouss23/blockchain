open Transaction
open Block

(* Montant de la recompense une fois un bloc mine *)
let miningReward = 10

(* Liste des block mines *)
let mined_blocks = ref []

(* difficulte de minning *)
let difficulty = 2

(* liste de transactions en attente de la blockchain *)
let pending_transactions = ref []

(* nombre de transactions par bloc *)
let transactions_per_block = 5

(* l'adresse qui recompense les miners *)
let miningRewardFromAddress = "block chain"

(* Fonction qui mine un block avec la liste des transactions en attente
On met l'adresse du mineur pour qu'il soit recompense *)
let minePendingTransactions miningRewardAdress = (
    let first_transactions = List.init transactions_per_block (fun i -> List.nth !pending_transactions i) in
    let remaining_transactions = List.init ((List.length !pending_transactions) - transactions_per_block)
                                            (fun i -> List.nth !pending_transactions (i + transactions_per_block)) in
    let current_block = make_block_w_transac (List.length !mined_blocks) first_transactions in
    let mined_block = puzzle current_block difficulty in
    mined_blocks := mined_block :: !mined_blocks;
    pending_transactions := (make_transaction miningRewardFromAddress miningRewardAdress miningReward) :: remaining_transactions;
    
    Format.printf "blocks %d finished with nonce %d@." current_block.id current_block.nonce;

    pending_transactions := List.init 1 (fun i -> make_transaction miningRewardFromAddress miningRewardAdress miningReward);
    print_transactions !pending_transactions;
)

(* Fonction pour pusher transaction dans pending_transactions *)
let addTransaction (current_transaction : transaction)= (
    pending_transactions := current_transaction :: !pending_transactions;
    if (List.length !pending_transactions) >= transactions_per_block then
    begin
        minePendingTransactions "miner"
    end
)

(* Fonction qui dit l'etat du compte d'une adresse dans une blockchain *)
let getBalanceOfAddress address = (
  let balance = ref 0 in
  List.iter (fun current_block ->
    Format.printf "Parse list of block : %d with balance %d @." current_block.id !balance;
    List.iter (fun transac ->
        Format.printf "fromAddress %s  toAddress %s amount %d@." transac.fromAddress transac.toAddress transac.amount;

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
