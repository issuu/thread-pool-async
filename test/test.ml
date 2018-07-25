open Core
open Async_kernel
open Async_unix

let unit_or_error =
  (* exact error does not matter *)
  Alcotest.result Alcotest.unit Alcotest.pass

let test_simple () =
  let threads = 10 in
  let work = 1000 in
  let states = ref 0 in
  let jobs = ref 0 in
  let create () = Int.incr states in
  let destroy () = Int.decr states in
  let worker () =
    let%bind () = after (Time.Span.of_ms 5.) in
    return @@ `Ok (Int.incr jobs)
  in

  let%bind pool = Thread_pool.init ~name:"unittest" ~threads ~create ~destroy in
  Alcotest.(check int) "Pool not initialized correctly" threads !states;
  (* Do a 1000 requests *)
  let%bind result = List.init work ~f:ignore |> Deferred.Or_error.List.iter ~how:`Parallel ~f:(fun () -> Thread_pool.with' pool worker) in
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
  let create () =
    Int.incr states;
    !states
  in
  let worker = function
    | n when Int.rem n 2 = 0 ->
      Int.incr errors;
      failwith "Stop"
    | _ ->
      let%bind () = after (Time.Span.of_ms 5.) in
      return @@ `Ok (Int.incr jobs)
  in

  let%bind pool = Thread_pool.init ~name:"unittest" ~threads ~create ~destroy:ignore in
  let%bind _result = List.init work ~f:ignore |> Deferred.Or_error.List.iter ~how:`Parallel ~f:(fun () -> Thread_pool.with' pool worker) in
  let%bind () = Thread_pool.destroy pool in

  Alcotest.(check int) "Threads not recreated correctly" !errors (!states - threads);
  return ()

let test_retry () =
  let threads = 10 in
  let work_expected = 1000 in
  let work_done = ref 0 in
  let creations_expected = threads + work_expected in
  let creations = ref 0 in
  let destructions_expected = threads + work_expected in
  let destructions = ref 0 in
  let create () = Int.incr creations in
  let destroy () = Int.incr destructions in
  let worker should_fail () =
    let result = match !should_fail with
      | true -> `Attempt_retry
      | false -> `Ok (Int.incr work_done)
    in
    should_fail := false;
    return result
  in
  let%bind pool = Thread_pool.init ~name:"unittest" ~threads ~create ~destroy in
  let%bind _result = List.init work_expected ~f:ignore |> Deferred.Or_error.List.iter ~how:`Parallel ~f:(fun () -> Thread_pool.with' ~retries:1 pool (worker (ref true))) in
  let%bind () = Thread_pool.destroy pool in

  Alcotest.(check int) "Work not done" work_expected !work_done;
  Alcotest.(check int) "Not correct amount of recreations" creations_expected !creations;
  Alcotest.(check int) "Not correct amount of destructions" destructions_expected !destructions;
  return ()

let test_retry_fail () =
  let threads = 10 in
  let work = 1000 in
  let tries = ref 0 in
  let create = ignore in
  let destroy = ignore in
  let worker () =
    Int.incr tries;
    return `Attempt_retry
  in
  let%bind pool = Thread_pool.init ~name:"unittest" ~threads ~create ~destroy in
  let%bind result = List.init work ~f:ignore |> Deferred.Or_error.List.iter ~how:`Parallel ~f:(fun () -> Thread_pool.with' ~retries:0 pool worker) in
  let%bind () = Thread_pool.destroy pool in

  Alcotest.(check int) "Tries but no retries" work !tries;
  Alcotest.(check unit_or_error) "Result should be an error" (Or_error.errorf "exact error does not matter") result;
  return ()

let test_set = [
  Alcotest_async.test_case "Simple" `Quick test_simple;
  Alcotest_async.test_case "Error" `Quick test_error;
  Alcotest_async.test_case "Retry" `Quick test_retry;
  Alcotest_async.test_case "Retry with no allowance" `Quick test_retry_fail;
]

let () =
  Alcotest.run Caml.__MODULE__ [
    ("test_set", test_set);
  ]
