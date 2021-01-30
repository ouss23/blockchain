

(*peers = wallet ou nodes*)

let () =
	(*mise en place de la socket*)
	setsockpot s SO_REUSEADDR true;
	bind s addr;
	listen s 5;
	(*connections aux peers*)
	connect_to_peer (); 


	while true do
		(*accepte connection socket*)
		let sc, _ = accept s in
		(*cannal entrée sortie*)
		let in_chan = in_channel_of_descr sc in
		let out_chan = out_channel_of_descr sc in
		let m = input_value in_chan in 
		(*condition messages*)
		match m with
		| Connect p ->
			Format.printf "Peers %d is connecting to P2P@" p;
			peers := S.add p ! peers;
			send out_chan !peers
		| Message s ->
			if already_received m then
				Format.printf "Already_received message : %s@." s;
			else
				begin
					Format.printf "Msg : %s@." s;
					add_message m;
					broadcast m
				end
			peers := S.add p ! peers;
			send out_chan !peers

		| ShowPeers ->
			print_peers ()
	done
