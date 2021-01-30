open Unix
type t =
	| Connect of int 
	| Message of string
	| ShowPeers (*pas utilse*)

let ip = inet inet_addr_of_string "127.0.0.1"
(*pas utilse*)
let send chan v =
	output_value chan v;
	flush chan
let connect_and_send d v =
	let in_chan, out_chan = open_connection (ADDR_INET(ip, d)) in
	send out_chan v;
	in_chan, out_chan