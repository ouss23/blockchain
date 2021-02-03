type block = { m : string; id : int; mutable nonce : int }
let make_b i m = { m = m; nonce = 0; id = i}
let blocks n = List.init n (fun i -> make_b i ("blabla"^(string_of_int i)))
let block_bools n = List.init n (fun i -> false)

(* calcule et retourne le hash en string d'un objet b *)
let get_hash b = Digest.string (Marshal.to_string b [])

(* nombre de 0 necessaires pour accepter un hash code *)
let difficulty = 2

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
let m = Mutex.create()
(* trouver un nonce qui permet d'avoir un hash code
de bloc commencant par la sequence de 0 *)
let rec mine_block bl_block bl_thread=
    if not bl_block.finished then
        let hash_code = get_hash bl_thread in
        if starts_with header hash_code then 
            Mutex.lock m;
            if not bl_block.finished then
                bl_block.finished = true
                bl_block.nonce = bl_thread.nonce
                bl_block.nonce
            else
                Format.printf "interruption";
                bl_block.nonce
            Mutex.unlock m;

        else
        begin
            bl_thread.nonce <- bl_thread.nonce + 1;

            mine_block bl_block bl_thread
        end
    else
        Format.printf "interruption";
        bl_block.nonce

(* miner une liste de blocs l,
en partant de l'element i,
avec un saut de "step" *)
let rec mine_blocks l i step t_id list_threads =
    if (i < List.length l) then
    begin
        let list_inf_thread = List.nth list_threads t_id in
        let bl_thread = List.nth list_inf_thread i in

        let bl_block = List.nth l i in
        let nonce = mine_block bl_block bl_thread in
        begin
            Format.printf "%s@." (Printf.sprintf "Block %d mined by thread %d, nonce is %d, hash is %s\n" i t_id nonce (get_hash bl_block));
            mine_blocks l (i+step) step t_id list_threads
        end
    end
    else Format.printf "Thread %d finished@." t_id

(*nombres blocks et Threads*)
let nbr_blocks = ref 8
let nbr_Threads = ref 2

(* on essaie sur une liste de 8 blocs *)
let block_list = blocks !nbr_blocks

(* Liste thread contenant * nbr_block non et finished*)
type inf_threads = { mutable list_block : block list ; finished : bool}
let make_inf_threads () = { list_block = blocks !nbr_blocks; finished = false}
let list_threads = List.init !nbr_Threads(fun i -> List.init !nbr_blocks (fun i -> make_inf_threads () ) )

let () =
    

    (* on reformule un peu la fonction mine_blocks pour qu'elle ne prend
    qu'un seul argument qui est l'id du thread *)
    let aux i = mine_blocks block_list 0 2 i list_threads in
    (* le premier thread va miner les blocs d'id pairs *)
    let _ = Thread.create aux 0 in
    (* le 2eme va miner les blocs d'id impairs *)
    let _ = Thread.create aux 1 in


    ignore(read_line ())
