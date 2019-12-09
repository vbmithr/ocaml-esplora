open Core
open Async
open Esplora
open Esplora_async

let wrap_request
    ?(timeout=Time.Span.of_int_sec 5)
    ?(speed=`Quick) n service =
  Alcotest_async.test_case ~timeout n speed begin fun () ->
    (Fastrest.request ?auth:None service) |>
    Deferred.Or_error.ignore |>
    Deferred.Or_error.ok_exn
  end

let wrap_request_light
    ?(timeout=Time.Span.of_int_sec 5)
    ?(speed=`Quick) n f =
  Alcotest_async.test_case ~timeout n speed begin fun () ->
    f () |>
    Deferred.Or_error.ignore |>
    Deferred.Or_error.ok_exn
  end

let sample_txid = `Hex "1a3ff83063c6c6b9a9eee407b11d64c137ad6cce2980bc255c4dcd1bb3546ef0"
let sample_addr = Base58.Bitcoin.of_string_exn c "1237GUyVTEAP4rQFw9FohzuyFgn6vqNH8D"

let main = [
  wrap_request "txid" (txid sample_txid) ;
  wrap_request "address" (address sample_addr) ;
  wrap_request "utxo" (utxo sample_addr) ;
  wrap_request "txnSummary" (txnSummary sample_addr) ;
  wrap_request "txnMempool" (txnMempool sample_addr) ;
  wrap_request "txnChain" (txnChain sample_addr) ;
]

let () =
  Logs.set_reporter (Logs_async_reporter.reporter ()) ;
  Logs.set_level (Some Info) ;
  Alcotest.run ~and_exit:false "esplora" [
    "main", main ;
  ]

