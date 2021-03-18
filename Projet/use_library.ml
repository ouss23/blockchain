
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


let create_private_key = (
    let private_key = cmd_to_list "python ../pythonkey/create_private_key.py" in
    String.concat "\n" private_key
  
  )

let create_public_key private_key = (
  let tmp = "python ../pythonkey/create_public_key.py '" ^ private_key ^"'" in
    let public_key = cmd_to_list  tmp in
    String.concat "\n" public_key 
  
  )

  let sign_transaction_method private_key message = (
    let signature = "python ../pythonkey/sign_transaction.py '"  ^ private_key ^ "' '" ^ message ^"'"  in
    let signature = cmd_to_list  signature in
    String.concat "\n" signature  
  
  )
  let verify_sign_method public_key message signature = (
  let cmd = "python ../pythonkey/verify_sign.py --public_key '"  ^ public_key ^ "' --message '" ^ message ^"' --signature '"  ^ signature ^ "'"    in
  let verif = cmd_to_list  cmd in
  let verif = String.concat "\n" verif in

  verif = "True"
  

)

let () = (



 let private_key = create_private_key in


 
 let public_key = create_public_key private_key in

 let message = "salut Oussama" in



 let sign_transaction = sign_transaction_method private_key message in



 let verify_sign = verify_sign_method public_key message sign_transaction in
 Printf.printf "icii%b\n" verify_sign;


 let verify_sign = verify_sign_method "public_key" message sign_transaction in
 Printf.printf "icii%b\n" verify_sign;








);