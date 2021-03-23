(*
	type d'un noeud de l'arbre de merkle
	E : noeud vide
	N(hash, l, r, lvl) : hash, fils et niveau
*)
type t = E | N of Z.t * t * t * int

let of_string s = Z.of_string_base 16 s

(* calculer le hashcode d'un objet m *)
let string_hash m =
	string_of_int (Hashtbl.hash m)

(* transformer un hashcode en Z.t *)
let hash m =
	of_string (string_hash m)

let empty = E

let make_leaf x = N(hash x, E, E ,0)

let make_node t1 t2 =
	match t1, t2 with
	| N(x1, _, _ , lvl1), N(x2,_,_, lvl2) -> let lvl = max lvl1 lvl2 + 1 in
		N(hash (Z.add x1 x2), t1, t2, lvl)
	| _ -> assert false
	
let rec fusion lt =
	match lt with
	| [] | [_] -> lt
	| t1 :: t2 :: s -> make_node t1 t2 :: fusion s
	
let rec merkel_of_list l =
	match l with
	| [] -> E
	| [t] -> t
	| _ -> merkel_of_list (fusion l)

(* construire un arbre de merkle a partir d'une liste d'empreintes *)
let make l =
	let l = List.map of_string l in
	merkel_of_list (List.map make_leaf l)

(* obtenir l'empreinte de la racine d'un arbre de merkle *)
let hash_root t =
	match t with
	| E -> raise Not_found
	| N(x,_,_,_) -> Z.format "%X" x
	
(* retourner une preuve (liste d'empreintes) de la transaction a la feuille "i" a partir d'un arbre de merkle *)
let rec proof t i =
	match t with
	| E | N(_, E, E, _) -> []
	| N(_, g, d, lvl) -> let b = 1 lsl (lvl - 1) in
		if i < b then (hash_root d)::(proof g i) else (hash_root g)::(proof d (i - b))
		
let to_hash_list l =
	List.map string_hash l

(* verifier si une preuve "pr" de la transaction "tr" est correcte *)
let authenticate tr pr root =
	let tr = of_string tr in
	let root = of_string root in
	let pr = List.map of_string pr in
	let x = List.fold_right (fun h x -> hash (Z.add h x)) pr (hash tr) in
	x = root