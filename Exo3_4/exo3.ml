type block = { m : string; id : int; mutable nonce : int }
let make_b i m = { m = m; nonce = 0; id = i}
let blocks n = List.init n (fun i -> make_b i ("blabla"^(string_of_int i)))

(* calcule et retourne le hash en string d'un objet b *)
let get_hash b = Digest.string (Marshal.to_string b [])

(* nombre de 0 necessaires pour accepter un hash code *)
let difficulty = 3

(* la sequence de 0 de l'entete *)
let header = String.make difficulty '0'

(* voir si s commence par sub *)
let starts_with _sub _s =
    let rec aux sub s i =
        if (i >= String.length s) then false
        else if (i < String.length sub) then
        begin
            if ((String.get sub i) = (String.get s i)) then
                aux sub s (i+1)
            else
                false
        end
        else true
    in
    aux _sub _s 0

(* trouver un nonce qui permet d'avoir un hash code
de bloc commencant par la sequence de 0 *)
let rec mine_block b =
    let hash_code = get_hash b in
    if starts_with header hash_code then b.nonce
    else
    begin
        b.nonce <- b.nonce + 1;
        mine_block b
    end

(* miner une liste de blocs l,
en partant de l'element i,
avec un saut de "step" *)
let rec mine_blocks l i step t_id =
    if (i < List.length l) then
    begin
        let bl = List.nth l i in
        let nonce = mine_block bl in
        begin
            Format.printf "%s@." (Printf.sprintf "Block %d mined by thread %d, nonce is %d, hash is %s\n" i t_id nonce (get_hash bl));
            mine_blocks l (i+step) step t_id 
        end
    end
    else Format.printf "Thread %d finished@." t_id

(* on essaie sur une liste de 8 blocs *)
let block_list = blocks 8

let () =
    (* on reformule un peu la fonction mine_blocks pour qu'elle ne prend
    qu'un seul argument qui est l'id du thread *)
    let aux i = mine_blocks block_list i 2 i in
    (* le premier thread va miner les blocs d'id pairs *)
    let _ = Thread.create aux 0 in
    (* le 2eme va miner les blocs d'id impairs *)
    let _ = Thread.create aux 1 in
    ignore(read_line ())
