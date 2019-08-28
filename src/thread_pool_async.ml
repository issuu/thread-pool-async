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
}

let try_in_thread ~thread f = try_with (fun () -> In_thread.run ~thread f)

let destroy_pool {reader; writer; thread_count; destroy; _} =
  let%bind states_destroyed =
    List.init thread_count ~f:ignore
    |> Deferred.List.map ~how:`Parallel ~f:(fun () ->
           match%bind Pipe.read reader with
           | `Eof -> return @@ Ok ()
           | `Ok {thread; state} -> try_in_thread ~thread (fun () -> destroy state))
    |> Deferred.map ~f:Result.all_unit
  in
  Pipe.close writer;
  match states_destroyed with
  | Ok () -> return ()
  | Error exn -> raise @@ State_manipulation_error exn

let recreate_state ({create; destroy; _} as pool) {thread; state} =
  let destroy_pool_and_raise exn =
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
      let pool = {reader; writer; create; destroy; thread_count} in
      let%bind threads =
        List.init thread_count ~f:ignore
        |> Deferred.List.map ~how:`Parallel ~f:(In_thread.Helper_thread.create ~name)
      in
      let create_state thread = try_in_thread ~thread create in
      match%bind
        threads
        |> Deferred.List.map ~how:`Parallel ~f:create_state
        |> Deferred.map ~f:Result.all
      with
      | Ok states ->
          let add_worker (thread, state) =
            Pipe.write_without_pushback writer {thread; state}
          in
          List.zip_exn threads states |> List.iter ~f:add_worker;
          return pool
      | Error exn ->
          let%bind () = destroy_pool pool in
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
