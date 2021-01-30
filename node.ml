open Unix
open Common

let port = ref 8000

let register = ref None
let set_register x = register := Some x

let options = [
    "-register", Arg.Int set_register, " Register to a peer";
]

let _ = Arg.parse options (fun i -> port := int_of_string i) "P2P"

(* Adresse IP *)
let addr = ADDR_INET(ip,!port)

(* création de la socket IPv4, TCP *)
let s = socket PF_INET SOCK_STREAM 0

module Int =
struct
    type t = int
    let compare = compare
end

module S = Set.Make(Int)

let peers = ref (S.singleton !port)

(* liste des messages, stockés sous forme de hash *)
let messages = ref S.empty

(* calcule et retourne le hash de m *)
let hash m = Hashtbl.hash m

(* ajoute un message (en hash) m à la liste des messages *)
let add_message m = messages := S.add (hash m) !messages

(* vérifie si un message m est déjà reçu (comparer avec les hash) *)
let already_received m = S.mem (hash m) !messages

let print_peers () =
    Format.printf "Peers : ";
    S.iter (Format.printf "%d ") !peers;
    Format.printf "@."

let connect_to_peer () =
    match !register with
        | None -> ()
        | Some r ->
            Format.printf "connecting to %d@." r;
            let in_chan, _ = connect_and_send r (Connect !port) in
            peers := input_value in_chan

let broadcast v =
    S.iter (fun p ->
        if p <> !port then let _ = connect_and_send p v in ()) !peers

let () =
    setsockopt s SO_REUSEADDR true;
    bind s addr;
    listen s 5;

    connect_to_peer ();

    while true do
        let sc, _ = accept s in
        let in_chan = in_channel_of_descr sc in
        let out_chan = out_channel_of_descr sc in
        let m = input_value in_chan in
        match m with
        | Connect p ->
            Format.printf "Peer %d is connecting to P2P@." p;
            peers := S.add p !peers;
            send out_chan !peers;
        | Message s ->
            if already_received m then
                Format.printf "Already received message : %s@." s
            else
                begin
                Format.printf "Msg : %s@." s;
                add_message m;
                broadcast m
            end
        | ShowPeers ->
            print_peers()
    done
