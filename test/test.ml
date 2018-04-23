open Base
open Async_kernel

let fmt_or_error to_sexp =
  let to_string v = Sexp.to_string_hum @@ Or_error.sexp_of_t to_sexp v in
  Fmt.of_to_string to_string
let eq_or_error compare a b =
  Or_error.compare compare a b = 0

let or_testable sexp_of_t compare =
  Alcotest.testable (fmt_or_error sexp_of_t) (eq_or_error compare)

let unit_or_error = or_testable Unit.sexp_of_t Unit.compare

let test_simple () =
  let threads = 10 in
  let work = 1000 in
  let states = ref 0 in
  let jobs = ref 0 in
  let create_w () = Int.incr states in
  let destroy_w () = Int.decr states in
  let worker () =
    Unix.sleepf 0.005;
    Int.incr jobs
  in

  let%bind pool = Thread_pool.init ~name:"unittest" ~threads ~create:create_w ~destroy:destroy_w in
  Alcotest.(check int) "Pool not initialized correctly" threads !states;
  (* Do a 1000 requests *)
  let%bind result = List.init work ~f:ignore |> Deferred.Or_error.List.iter ~how:`Parallel ~f:(fun () -> Thread_pool.with_ pool ~f:worker) in
  Alcotest.(check unit_or_error) "Not all jobs completed without error" (Or_error.return ()) result;
  Alcotest.(check int) "Not all jobs ran" work !jobs;

  let%bind () = Thread_pool.destroy pool in
  Alcotest.(check int) "Not all workers destroyed" 0 !states;
  return ()

let test_error () =
  let threads = 10 in
  let work = 1000 in
  let states = ref 0 in
  let jobs = ref 0 in
  let errors = ref 0 in
  let create_w () =
    Int.incr states;
    !states
  in
  let worker = function
    | n when Int.rem n 2 = 0 ->
      Int.incr errors;
      failwith "Stop"
    | _ ->
      Unix.sleepf 0.005;
      Int.incr jobs
  in

  let%bind pool = Thread_pool.init ~name:"unittest" ~threads ~create:create_w ~destroy:ignore in
  let%bind _result = List.init work ~f:ignore |> Deferred.Or_error.List.iter ~how:`Parallel ~f:(fun () -> Thread_pool.with_ pool ~f:worker) in
  let%bind () = Thread_pool.destroy pool in

  Alcotest.(check int) "Threads not recreated correctly" !errors (!states - threads);
  return ()

let test_set = [
  Alcotest_async.test_case "Simple" `Slow test_simple;
  Alcotest_async.test_case "Error" `Slow test_error;
]

let () =
  Alcotest.run Caml.__MODULE__ [
    ("test_set", test_set);
  ]