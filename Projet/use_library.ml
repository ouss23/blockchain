
open Unix
let process_output_to_list2 = fun command -> 
  let chan = Unix.open_process_in command in
  let res = ref ([] : string list) in
  let rec process_otl_aux () =  
    let e = input_line chan in
    res := e::!res;
    process_otl_aux() in
  try process_otl_aux ()
  with End_of_file ->
    let stat = Unix.close_process_in chan in (List.rev !res,stat)
let cmd_to_list command =
  let (l,_) = process_output_to_list2 command in l



let print_jsp transactions_list = 
  (List.iter (fun transac ->
                  Format.printf "fromAddress %s." transac;
              ) transactions_list;
  )




let () = (



 let private_key = cmd_to_list "python ../pythonkey/create_private_key.py"in



 let private_key = String.concat "\n" private_key in 






 Printf.printf "icii%s\n" private_key;


 
 let public_key = "python ../pythonkey/create_public_key.py '"  ^ private_key ^"'" in

 Printf.printf "icii%s\n" public_key;



 let public_key = cmd_to_list public_key in

 let public_key = String.concat "\n" public_key in 

 Printf.printf "icii last%s\n" public_key;

 let message = "salut Oussama" in
 
 let sign_transaction = "python ../pythonkey/sign_transaction.py '"  ^ private_key ^ "' '" ^ message ^"'" in
 Printf.printf "icii%s\n" sign_transaction;


 let sign_transaction = cmd_to_list sign_transaction in

 let sign_transaction = String.concat "\n" sign_transaction in 

 Printf.printf "icii signature %s\n" sign_transaction;

 let verify_sign = "python ../pythonkey/verify_sign.py --public_key '"  ^ public_key ^ "' --message '" ^ message ^"' --signature '"  ^ sign_transaction ^ "'"    in

 Printf.printf "icii%s\n" verify_sign;


 let verify_sign = cmd_to_list verify_sign in

 let verify_sign = String.concat "\n" verify_sign in 

 Printf.printf "icii verify %s\n" verify_sign;



 ()


);