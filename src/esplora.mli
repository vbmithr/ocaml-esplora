open Json_encoding

val c : (module Base58.CRYPTO)
val hex : Hex.t encoding

module Transaction : sig
  type version =
    | Legacy of Base58.Bitcoin.version
    | Segwit of int

  type address =
    | B58 of Base58.Bitcoin.t
    | B32 of [`Btc] Bech32.Segwit.t

  type vout = {
    spk: Hex.t ;
    spk_asm : string ;
    spk_type : version ;
    spk_addr : address ;
    value : int64 ;
  }

  type status = {
    block_height : int ;
    block_hash : Hex.t ;
    block_time : Ptime.t ;
  }

  type vin = {
    txid: Hex.t ;
    vout: int ;
    prevout: vout ;
    is_coinbase: bool;
    witness: Hex.t list ;
    seq: int64 ;
    scriptsig: Hex.t ;
    scriptsig_asm : string ;
    inner_redeemscript_asm : string option ;
    inner_witnessscript_asm : string option ;
  }

  type locktime = [`Block of int | `Time of Ptime.t]

  type t = {
    txid: Hex.t ;
    version: int ;
    locktime: locktime ;
    vin: vin list ;
    vout: vout list ;
    size: int ;
    weight: int ;
    fee: int ;
    status: status option ;
  }

  val t : t encoding
end

module Address : sig
  type stats = {
    funded_txo_count: int ;
    funded_txo_sum: int64 ;
    spent_txo_count: int ;
    spent_txo_sum : int64 ;
    tx_count : int ;
  }

  type t = {
    address: Base58.Bitcoin.t ;
    chain_stats: stats ;
    mempool_stats: stats ;
  }

  val t : t encoding
end
