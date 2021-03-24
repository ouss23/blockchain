# Projet UE15 - Blockchain

## Utilisation en local
### Dépendences
Pour pouvoir compiler et exécuter la mini-blockchain, il faut avoir les bibliothèques suivantes :
- Zarith pour OCaml (installé avec opam)
- Crypto / Cryptography / Cryptodome pour Python

Pour installer Crypto et toutes ses dépendances, vous pouvez créer un virtualenv et installer les dépendences listé dans `requirements.txt`

### Compilation
Vous pouvez compiler le code avec ocamlfind et ocamlc en exécutant le script shell `./Projet/compile.sh`

### Exécution
La compilation génère 2 binaires, un pour les miners et un pour les wallets.
Pour lancer un miner qui écoute sur le port 8000, exécuter :
`cd Projet
./miner 8000`

Pour lancer un autre miner écoutant sur le port 9000 et connecté au miner précedent, exécuter :
`./miner 9000 -register 8000`

Pour lancer un wallet connecté au miner occupant le port 8000, exécuter :
`./wallet 8000`

### Tests
Pour lancer une suite de 4 cas de tests sur les transactions, lancer un miner sur un port (8000 par ex.) et exécuter :
`./tester 8000`

## Architecture du projet
![Architecture](https://i.ibb.co/PT3ygSK/diagram-classes.png)

## Fonctionnement du miner
![Miner](https://i.ibb.co/f9tfhKh/miner.png)
