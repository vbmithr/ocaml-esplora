open Esplora

val base_url : Uri.t ref

val txid : Hex.t -> (Fastrest.form, Transaction.t) Fastrest.service
val address : Base58.Bitcoin.t -> (Fastrest.form, Address.t) Fastrest.service
val utxo : Base58.Bitcoin.t -> (Fastrest.form, Address.t list) Fastrest.service
val txnSummary : Base58.Bitcoin.t -> (Fastrest.form, Transaction.t list) Fastrest.service
val txnChain : ?lastTxid:Hex.t -> Base58.Bitcoin.t -> (Fastrest.form, Transaction.t list) Fastrest.service
val txnMempool : Base58.Bitcoin.t -> (Fastrest.form, Transaction.t list) Fastrest.service
