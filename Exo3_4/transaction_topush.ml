(* Transactions *)
type transactions = { fromAddress : string; toAddress : string; amount : int}
let make_transaction fromAddress_arg toAddress_arg amount_arg = { fromAddress = fromAddress_arg; toAddress = toAddress_arg; amount = amount_arg}

(* Chaque block a une liste de list_transactions *)
type block = { id : int; mutable nonce : int; mutable list_transactions :  transactions list}
let make_block_w_transac id_arg list_transactions_arg = {id=id_arg; nonce = 0; list_transactions=list_transactions_arg}


(* Montant de la recompense une fois un block miné *)
let miningReward = ref 10;; 

(* Liste des block miné*)
let block_mined = ref []
let difficulty = 2

(* Mutex*)
let m = Mutex.create()

(* Fonction de verification*)
let check d =
 (String.sub d 0 difficulty) = String.make difficulty '\000'

(* Pour trouver le bon nonce*)
let rec puzzle b =
 let d = Digest.string (Marshal.to_string b []) in 
 if check d then b
 else
  puzzle { b with nonce = b.nonce + 1 }

(*Fonction pour miner un block*)

let mine_one old_b = (
  
    let b = puzzle old_b in
 
    Mutex.lock m;
    block_mined := puzzle b :: !block_mined; 
    old_b.nonce <-  b.nonce ; 
    Mutex.unlock m;
    )
 



(*Fonction pour afficher liste de transactions*)
let print_list list = 
  (List.iter
(fun transac ->
  Format.printf "fromAddress %s  toAddress %s amount %d@." transac.fromAddress transac.toAddress transac.amount;

  )list;
  )

(* liste de transactions en attente de la blockchain*)
let blockchain_pending_transactions = ref []

(* Fonction pour pusher transaction dans blockchain_pending_transactions*)
let createTransactions current_transaction = (
  blockchain_pending_transactions := current_transaction :: !blockchain_pending_transactions 
)


(* Fonction qui mine un block avec la liste des transactions en attente
On met l'adresse du mineur pour qu'il soit récompensé
*)
let minePendingTransactions miningRewardAdress = (
  let miningRewardAdress2 = ref ("adress_block_chain") in

  let current_block = make_block_w_transac 0 !blockchain_pending_transactions in
  mine_one (current_block);


  Format.printf "blocks %d finished with nonce %d@." current_block.id current_block.nonce;

  blockchain_pending_transactions := List.init 1 (fun i -> make_transaction !miningRewardAdress2 !miningRewardAdress !miningReward);
  print_list !blockchain_pending_transactions;

  
)

(* Fonction qui dit l'état du compte d'une adresse dans une blockChain
*)

let getBalanceOfAdress adress = (
  let balance = ref 0 in
  List.iter
  (fun current_block ->
    Format.printf "Parse list of block : %d with balance %d @." current_block.id !balance;
    List.iter
    (fun transac ->
      Format.printf "fromAddress %s  toAddress %s amount %d@." transac.fromAddress transac.toAddress transac.amount;

      if adress = transac.fromAddress then
        begin
          balance := !balance - transac.amount
        end;

      if adress = transac.toAddress then
        begin
          balance := !balance + transac.amount
        end;
    )current_block.list_transactions
  

  )!block_mined;

  balance
)


let () =
(* adress1" envoies 100 bitcoin à "adress2*)

  let first_transac = make_transaction "adress1" "adress2" 100 in
  createTransactions first_transac;

(* adress2" envoies 50 bitcoin à "adress1*)

  let second_transac = make_transaction "adress2" "adress1" 50 in
  createTransactions second_transac;
(* On mine *)
  Format.printf "Stating the miner...\n";

  let miner1_adress = ref "miner1_adress" in
  minePendingTransactions miner1_adress;

  Format.printf "Mined finished\n";
(* L'état du compte  du mineur est 0 car sa recompense n'as pas été encore miner*)
  let balance_miner1 = getBalanceOfAdress !miner1_adress in
  Format.printf "Balance of %s is %d@.\n" !miner1_adress !balance_miner1;
(* On mine donc a nouveau *)

  Format.printf "\nStating the miner again...\n";

  let miner1_adress = ref "miner1_adress" in
  minePendingTransactions miner1_adress;

  Format.printf "Mined finished\n";

(* L'état du compte  du mineur est à 10  et j'aficche aussi pour adress 1 et 2*)

  let balance_miner1 = getBalanceOfAdress !miner1_adress in
  Format.printf "Balance of %s is %d@.\n" !miner1_adress !balance_miner1;

  let balance_adress1 = getBalanceOfAdress "adress1" in
  Format.printf "Balance of %s is %d@.\n" "adress1" !balance_adress1;

  let balance_adress2 = getBalanceOfAdress "adress2" in
  Format.printf "Balance of %s is %d@.\n" "adress2" !balance_adress2;





  (*blockchain_pending_transactions = [  ] ;

  let new_blockchain_pending_transactions = ref [(make_transaction -1 miningRewardAdress miningRewardAdress)]*)


  Format.printf "Block miné = %d @." (List.length !block_mined) 