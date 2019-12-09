open Json_encoding

module Crypto = struct
  let sha256 s = Digestif.SHA256.(to_raw_string (digest_string s))
end

let c = (module Crypto : Base58.CRYPTO)

let hex = conv (fun (`Hex h) -> h) (fun h -> `Hex h) string

module Transaction = struct
  type version =
    | Legacy of Base58.Bitcoin.version
    | Segwit of int

  type address =
    | B58 of Base58.Bitcoin.t
    | B32 of [`Btc] Bech32.Segwit.t

  let base58 =
    conv (Base58.Bitcoin.to_string c) (Base58.Bitcoin.of_string_exn c) string

  let bech32 =
    conv Bech32.Segwit.encode_exn (Bech32.Segwit.decode_exn (module Bech32.Segwit.Btc)) string

  let addr =
    union [
      case base58 (function B58 a -> Some a | _ -> None) (function a -> B58 a) ;
      case bech32 (function B32 a -> Some a | _ -> None) (function a -> B32 a) ;
    ]

  let version =
    string_enum [
      "p2sh", Legacy Base58.Bitcoin.P2SH ;
      "p2pkh", Legacy P2PKH ;
      "v0_p2wpkh", Segwit 0 ;
    ]

  type vout = {
    spk: Hex.t ;
    spk_asm : string ;
    spk_type : version ;
    spk_addr : address ;
    value : int64 ;
  }

  let vout =
    conv
      (fun _ -> assert false)
      (fun (spk, spk_asm, spk_type, spk_addr, value) ->
         { spk ; spk_asm ; spk_type ; spk_addr ; value })
      (obj5
         (req "scriptpubkey" hex)
         (req "scriptpubkey_asm" string)
         (req "scriptpubkey_type" version)
         (req "scriptpubkey_address" addr)
         (req "value" int53))

  let ptime =
    conv (fun _ -> assert false) (fun s -> Option.get (Ptime.of_float_s s)) float

  type status = {
    block_height : int ;
    block_hash : Hex.t ;
    block_time : Ptime.t ;
  }

  let status =
    conv
      (fun _ -> assert false)
      (fun (_, block_height, block_hash, block_time) ->
         { block_height ; block_hash ; block_time })
      (obj4
         (req "confirmed" bool)
         (req "block_height" int)
         (req "block_hash" hex)
         (req "block_time" ptime))

  let maybe_status =
    union [
      case (obj1 (req "confirmed" bool)) (fun _ -> None) (fun _ -> None)  ;
      case status (fun a -> a) (fun a -> Some a) ;
    ]

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

  let vin =
    conv
      (fun _ -> assert false)
      (fun (txid, vout, prevout, is_coinbase, witness, seq, scriptsig, scriptsig_asm,
            inner_redeemscript_asm, inner_witnessscript_asm) ->
        { txid; vout; prevout; is_coinbase; witness; seq; scriptsig; scriptsig_asm;
          inner_redeemscript_asm; inner_witnessscript_asm })
      (obj10
         (req "txid" hex)
         (req "vout" int)
         (req "prevout" vout)
         (req "is_coinbase" bool)
         (dft "witness" (list hex) [])
         (req "sequence" int53)
         (req "scriptsig" hex)
         (req "scriptsig_asm" string)
         (opt "inner_redeemscript_asm" string)
         (opt "inner_witnessscript_asm" string))

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

  let locktime =
    conv (fun _ -> assert false)
      (fun i -> if i < 500000000. then `Block (Int.of_float i) else `Time (Option.get (Ptime.of_float_s i)))
      float

  let t =
    conv
      (fun _ -> assert false)
      (fun (txid, version, locktime, vin, vout, size, weight, fee, status) ->
         { txid; version; locktime; vin; vout; size; weight; fee; status })
      (obj9
         (req "txid" hex)
         (req "version" int)
         (req "locktime" locktime)
         (req "vin" (list vin))
         (req "vout" (list vout))
         (req "size" int)
         (req "weight" int)
         (req "fee" int)
         (req "status" maybe_status))
end

module Address = struct
  type stats = {
    funded_txo_count: int ;
    funded_txo_sum: int64 ;
    spent_txo_count: int ;
    spent_txo_sum : int64 ;
    tx_count : int ;
  }

  let stats =
    conv
      (fun _ -> assert false)
      (fun (funded_txo_count, funded_txo_sum, spent_txo_count, spent_txo_sum, tx_count) ->
         { funded_txo_count; funded_txo_sum; spent_txo_count; spent_txo_sum; tx_count })
      (obj5
         (req "funded_txo_count" int)
         (req "funded_txo_sum" int53)
         (req "spent_txo_count" int)
         (req "spent_txo_sum" int53)
         (req "tx_count" int))

  type t = {
    address: Base58.Bitcoin.t ;
    chain_stats: stats ;
    mempool_stats: stats ;
  }

  let address =
    conv (Base58.Bitcoin.to_string c) (Base58.Bitcoin.of_string_exn c) string

  let t =
    conv
      (fun _ -> assert false)
      (fun (address, chain_stats, mempool_stats) -> { address ; chain_stats ; mempool_stats })
      ((obj3)
         (req "address" address)
         (req "chain_stats" stats)
         (req "mempool_stats" stats))
end
