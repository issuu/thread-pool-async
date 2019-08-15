open Core
open Async_kernel
module Thread_pool = Thread_pool_async

let jobs_to_launch = 1000

let launch_jobs ?(retries = 0) pool get_work =
  let launch_job () =
    let work = get_work () in
    Thread_pool.with' ~retries pool work
  in
  List.init jobs_to_launch ~f:ignore
  |> Deferred.List.map ~how:`Parallel ~f:launch_job
  |> Deferred.map ~f:Result.all_unit

let test_simple () =
  let threads = 10 in
  let states = ref 0 in
  let jobs = ref 0 in
  let create () = Int.incr states in
  let destroy () = Int.decr states in
  let work () =
    ignore @@ Unix.nanosleep 0.005;
    Ok (Int.incr jobs)
  in
  let%bind pool = Thread_pool.init ~name:"unittest" ~threads ~create ~destroy in
  Alcotest.(check int) "Pool not initialized correctly" threads !states;
  let%bind result = launch_jobs pool (fun () -> work) in
  Alcotest.(check @@ result unit unit)
    "Not all jobs completed without error"
    (Ok ())
    result;
  Alcotest.(check int) "Not all jobs ran" jobs_to_launch !jobs;
  let%bind () = Thread_pool.destroy pool in
  Alcotest.(check int) "Not all states destroyed" 0 !states;
  return ()

let test_error () =
  let threads = 10 in
  let states = ref 0 in
  let jobs = ref 0 in
  let errors = ref 0 in
  let create () = Int.incr states; !states in
  let work = function
    | n when Int.rem n 2 = 0 -> Int.incr errors; failwith "Stop"
    | _ ->
        ignore @@ Unix.nanosleep 0.005;
        Ok (Int.incr jobs)
  in
  let%bind pool = Thread_pool.init ~name:"unittest" ~threads ~create ~destroy:ignore in
  let%bind result =
    try_with (fun () -> launch_jobs pool (fun () -> work))
    |> Deferred.Result.map_error ~f:ignore
  in
  Alcotest.(check (result reject unit))
    "Execution expected to fail but it succeeded"
    (Error ())
    result;
  let%bind () = Thread_pool.destroy pool in
  Alcotest.(check int) "States not recreated correctly" !errors (!states - threads);
  return ()

let test_retry () =
  let threads = 10 in
  let work_done = ref 0 in
  let creations_expected = threads + jobs_to_launch in
  let creations = ref 0 in
  let destructions_expected = threads + jobs_to_launch in
  let destructions = ref 0 in
  let create () = Int.incr creations in
  let destroy () = Int.incr destructions in
  let get_work () =
    let work should_fail () =
      let result =
        match !should_fail with
        | true -> Error ()
        | false -> Ok (Int.incr work_done)
      in
      should_fail := false;
      result
    in
    work (ref true)
  in
  let%bind pool = Thread_pool.init ~name:"unittest" ~threads ~create ~destroy in
  let%bind result = launch_jobs pool ~retries:1 get_work in
  Alcotest.(check (result unit unit))
    "Execution expected to succeed but it failed"
    (Ok ())
    result;
  let%bind () = Thread_pool.destroy pool in
  Alcotest.(check int) "Work not done" jobs_to_launch !work_done;
  Alcotest.(check int) "Not correct amount of recreations" creations_expected !creations;
  Alcotest.(check int)
    "Not correct amount of destructions"
    destructions_expected
    !destructions;
  return ()

let test_retry_fail () =
  let threads = 10 in
  let tries = ref 0 in
  let create = ignore in
  let destroy = ignore in
  let work () = Int.incr tries; Error () in
  let%bind pool = Thread_pool.init ~name:"unittest" ~threads ~create ~destroy in
  let%bind result = launch_jobs pool (fun () -> work) in
  let%bind () = Thread_pool.destroy pool in
  Alcotest.(check int) "Tries but no retries" jobs_to_launch !tries;
  Alcotest.(check @@ result unit unit) "Result should be an error" (Error ()) result;
  return ()

let test_interleaving_work_with_async_scheduler () =
  let threads = 10 in
  let states = ref 0 in
  let create () = Int.incr states; !states in
  let destroy = ignore in
  let%bind pool = Thread_pool.init ~name:"unittest" ~threads ~create ~destroy in
  let%bind result =
    Thread_pool.with_worker pool (fun worker ->
        let%bind worker_state_1 = Thread_pool.execute worker Fn.id in
        let%bind () = Async_kernel.after (Time_ns.Span.of_ms 5.) in
        let%bind worker_state_2 = Thread_pool.execute worker Fn.id in
        return
        @@
        match worker_state_1 = worker_state_2 with
        | true -> Ok ()
        | false -> Error ())
  in
  let%bind () = Thread_pool.destroy pool in
  Alcotest.(check (result unit unit)) "Worker changed state between uses" (Ok ()) result;
  return ()

let test_set =
  [ Alcotest_async.test_case "Simple" `Quick test_simple;
    Alcotest_async.test_case "Error" `Quick test_error;
    Alcotest_async.test_case "Retry" `Quick test_retry;
    Alcotest_async.test_case "Retry with no allowance" `Quick test_retry_fail;
    Alcotest_async.test_case
      "Interleave Async work with work on the pool"
      `Quick
      test_interleaving_work_with_async_scheduler ]

let () = Alcotest.run Caml.__MODULE__ ["test_set", test_set]
