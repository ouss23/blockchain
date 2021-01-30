open Unix
open Common

let port = ref 8000

let _ = Arg.parse [] (fun i -> port := int_of_string i) "Wallet"

let () =
    while true do
        Format.printf "> %!";
        match read_line() with
        | "show" -> let _ = connect_and_send !port ShowPeers in ()
        | "end" -> exit 0
        | n -> let _ = connect_and_send !port (Message m) in ()
    done
