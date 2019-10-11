open Base
open Async_kernel
open Async_unix

type 'state worker = {
  thread : In_thread.Helper_thread.t;
  state : 'state;
}

exception Invalid_thread_count of int

exception Pool_already_destroyed

exception State_manipulation_error of exn

type 'state t = {
  reader : 'state worker Pipe.Reader.t;
  writer : 'state worker Pipe.Writer.t;
  create : unit -> 'state;
  destroy : 'state -> unit;
  thread_count : int;
  (* Used to prevent rare race conditions when the pool is getting destroyed,
     especially when state recreation fails for multiple workers concurrently. *)
  destruction_in_progress : unit Deferred.t Ivar.t;
}

let try_in_thread ~thread f = try_with (fun () -> In_thread.run ~thread f)

let destroy_pool {reader; writer; thread_count; destroy; destruction_in_progress; _} =
  match Ivar.peek destruction_in_progress with
  | Some destruction_done ->
      (* wait upon the termination of the "other" destruction in progress *)
      destruction_done
  | None -> (
      let fill_when_done = Ivar.create () in
      let destruction_done = Ivar.read fill_when_done in
      Ivar.fill destruction_in_progress destruction_done;
      let%bind states_destroyed =
        List.init thread_count ~f:ignore
        |> Deferred.List.map ~how:`Parallel ~f:(fun () ->
               match%bind Pipe.read reader with
               | `Eof -> return @@ Ok ()
               | `Ok {thread; state} -> try_in_thread ~thread (fun () -> destroy state))
        |> Deferred.map ~f:Result.all_unit
      in
      Pipe.close writer;
      Ivar.fill fill_when_done ();
      match states_destroyed with
      | Ok () -> return ()
      | Error exn -> raise @@ State_manipulation_error exn)

let recreate_state ({create; destroy; writer; _} as pool) ({thread; state} as worker) =
  let destroy_pool_and_raise exn =
    Pipe.write_without_pushback writer worker;
    let%bind () = destroy_pool pool in
    raise @@ State_manipulation_error exn
  in
  match%bind try_in_thread ~thread (fun () -> destroy state) with
  | Ok () -> (
      match%bind try_in_thread ~thread create with
      | Ok state -> return {thread; state}
      | Error exn -> destroy_pool_and_raise exn)
  | Error exn -> destroy_pool_and_raise exn

let init ~name ~threads:thread_count ~create ~destroy =
  match thread_count with
  | n when n <= 0 -> raise @@ Invalid_thread_count thread_count
  | _ -> (
      (* No strict ordering on the heap. All items are equal *)
      let reader, writer = Pipe.create () in
      let destruction_in_progress = Ivar.create () in
      let pool =
        {reader; writer; create; destroy; thread_count; destruction_in_progress}
      in
      let create_state () =
        let%bind thread = In_thread.Helper_thread.create ~name () in
        try_in_thread ~thread create
        |> Deferred.Result.map ~f:(fun state -> thread, state)
      in
      let%bind worker_results =
        List.init thread_count ~f:ignore
        |> Deferred.List.map ~how:`Parallel ~f:create_state
      in
      match Result.all worker_results with
      | Ok workers ->
          let add_worker (thread, state) =
            Pipe.write_without_pushback writer {thread; state}
          in
          List.iter workers ~f:add_worker;
          return pool
      | Error exn ->
          let destroy_if_created = function
            | Ok (thread, state) -> try_in_thread ~thread (fun () -> destroy state)
            | Error _ -> return @@ Ok ()
          in
          let%bind () =
            worker_results
            |> List.map ~f:destroy_if_created
            |> Deferred.all
            |> Deferred.map ~f:ignore
          in
          Pipe.close writer;
          raise @@ State_manipulation_error exn)

let execute {thread; state} work = In_thread.run ~thread (fun () -> work state)

let with_worker ({reader; writer; _} as pool) ?(retries = 0) job =
  let run_once worker =
    let%bind result = try_with (fun () -> job worker) in
    let%bind worker =
      match result with
      | Ok (Ok _) -> return worker
      | Ok (Error _) | Error _ -> recreate_state pool worker
    in
    return (result, worker)
  in
  let rec run_with_retry worker retries_left =
    let%bind outcome, worker = run_once worker in
    match outcome with
    | Ok (Ok _) -> return (outcome, worker)
    | Ok (Error _) -> (
      match retries_left with
      | n when n <= 0 -> return (outcome, worker)
      | n -> run_with_retry worker (Int.pred n))
    | Error _ -> return (outcome, worker)
  in
  match%bind Pipe.read reader with
  | `Eof -> raise Pool_already_destroyed
  | `Ok worker -> (
      let%bind outcome, worker = run_with_retry worker retries in
      Pipe.write_without_pushback writer worker;
      match outcome with
      | Ok outcome -> return outcome
      | Error exn -> raise exn)

let with' pool ?(retries = 0) work =
  let job worker = execute worker work in
  with_worker pool ~retries job

let destroy = destroy_pool
