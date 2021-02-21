(* type de transactions *)
type transaction = { fromAddress : string; toAddress : string; amount : int}

(* creer une transaction *)
let make_transaction fromAddress_arg toAddress_arg amount_arg = { fromAddress = fromAddress_arg; toAddress = toAddress_arg; amount = amount_arg}

(* afficher une liste de transactions *)
let print_transactions transactions_list = 
    (List.iter (fun transac ->
                    Format.printf "fromAddress %s  toAddress %s amount %d@." transac.fromAddress transac.toAddress transac.amount;
                ) transactions_list;
    )


