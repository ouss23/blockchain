open Unix
(*Fonction pour executer une commande d'un script
 puis transformer ce que cette commande affiche sur l'output
 en une liste de chaine de charactere
 *)
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

(*Creer une clé privé et la retourne sous forme de chaine de charactère*)
let create_private_key () = (
    let private_key = cmd_to_list "python ../pythonkey/create_private_key.py" in
    String.concat "\n" private_key
  
  )
(*Creer une clé public a partir d'un clé privée et la retourne sous forme de chaine de charactère*)
let create_public_key private_key = (
  let tmp = "python ../pythonkey/create_public_key.py '" ^ private_key ^"'" in
    let public_key = cmd_to_list  tmp in
    String.concat "\n" public_key 
  
  )
(*Permet de signer un message et renvoie ce message signé (c'est à dire qu'il est crypter))*)
let sign_transaction_method private_key message = (
  let signature = "python ../pythonkey/sign_transaction.py '"  ^ private_key ^ "' '" ^ message ^"'"  in
  let signature = cmd_to_list  signature in
  String.concat "\n" signature  

)

(*Prend une clé publique, un message, et un message signé (message crypté).
Renvoie true si le message signé  qui est décrypté avec la clé publique correspond au message.
Cela permet de verifier si l'autheur du message est bien la même personne que le créteur de la clé publique*)

let verify_sign_method public_key message signature = (
  let cmd = "python ../pythonkey/verify_sign.py --public_key '"  ^ public_key ^ "' --message '" ^ message ^"' --signature '"  ^ signature ^ "'"    in
  let verif = cmd_to_list  cmd in
  let verif = String.concat "\n" verif in

  verif = "True"
  

)
