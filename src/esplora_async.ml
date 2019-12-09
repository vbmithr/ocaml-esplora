open Core
open Esplora
open Json_encoding

let base_url = ref @@ Uri.make ~scheme:"https" ~host:"blkhub.net" ~path:"api" ()

let ok e = conv (fun _ -> assert false) (fun v -> Ok v) e

let append_path url path =
  let p = Uri.path url in
  Uri.with_path url String.(concat ~sep:"/" (split p ~on:'/' @ path))

let txid (`Hex txid) =
  Fastrest.get (ok Transaction.t) (append_path !base_url ["tx"; txid])

let address addr =
  Fastrest.get (ok Address.t) (append_path !base_url ["address"; Base58.Bitcoin.to_string c addr])

let utxo addr =
  Fastrest.get (ok (list Address.t)) (append_path !base_url ["address"; Base58.Bitcoin.to_string c addr; "utxo"])

let txnSummary addr =
  Fastrest.get (ok (list Transaction.t)) (append_path !base_url ["address"; Base58.Bitcoin.to_string c addr; "txs"])

let txnMempool addr =
  Fastrest.get (ok (list Transaction.t)) (append_path !base_url ["address"; Base58.Bitcoin.to_string c addr; "txs"; "mempool"])

let txnChain ?lastTxid addr =
  Fastrest.get (ok (list Transaction.t))
    (append_path !base_url
       (["address"; Base58.Bitcoin.to_string c addr; "txs"; "chain"] @
        (match lastTxid with Some `Hex a -> [a]| None -> [])))
