
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


let () = (


(*


  let private_key_cm = Unix.open_process_in "python ../pythonkey/create_private_key.py"in

  let private_key = (input_line private_key_cm) in
  Printf.printf "%s\n" private_key;
  Unix.close_process_in private_key_cm;

  let str = "python ../pythonkey/create_public_key.py" ^  private_key in 
  Printf.printf "%s\n" str;





  let public_cm = Unix.open_process_in "python ../pythonkey/create_public_key.py" ^  private_key in


  try
    while true do
      let private_key = (input_line public_cm) in
      Printf.printf "%s\n" private_key
    done
  with End_of_file -> ();
  Unix.close_process_in public_cm;
  ();

  *)

 let jsp = cmd_to_list "python ../pythonkey/create_private_key.py"in
 let jsp2 = List.nth jsp 0 in
 Printf.printf "icii%s\n" jsp2;

 ()
  

);