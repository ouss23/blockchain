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

(* Fonction pour pusher transaction dans pending_transactions *)
let addTransaction current_transaction = (
  pending_transactions := current_transaction :: !pending_transactions 
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
