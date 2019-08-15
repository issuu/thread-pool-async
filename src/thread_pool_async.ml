open Base
open Async_kernel
open Async_unix

type 'state worker = {
  thread : In_thread.Helper_thread.t;
  state : 'state;
}

exception Invalid_thread_count of int

exception Pool_already_destroyed

type 'state t = {
  pool : 'state worker Pipe.Reader.t * 'state worker Pipe.Writer.t;
  create : unit -> 'state;
  destroy : 'state -> unit;
  threads : int;
}

let init ~name ~threads ~create ~destroy =
  match threads with
  | _ when threads <= 0 -> raise @@ Invalid_thread_count threads
  | _ ->
      let create_thread ~name ~create =
        let%bind thread = In_thread.Helper_thread.create ~name () in
        let%bind state = In_thread.run ~thread create in
        return {thread; state}
      in
      (* No strict ordering on the heap. All items are equal *)
      let reader, writer = Pipe.create () in
      let elements = List.init threads ~f:ignore in
      let add_to_pool () =
        create_thread ~name ~create >>| Pipe.write_without_pushback writer
      in
      let%bind () = Deferred.List.iter elements ~how:`Parallel ~f:add_to_pool in
      return {pool = reader, writer; create; destroy; threads}

let execute {thread; state} work = In_thread.run ~thread (fun () -> work state)

let with_worker {pool = reader, writer; destroy; create; _} ?(retries = 0) job =
  let recreate {thread; state} =
    let%bind () = In_thread.run ~thread (fun () -> destroy state) in
    let%bind state = In_thread.run ~thread create in
    return @@ {thread; state}
  in
  let run_once worker =
    let%bind result = try_with (fun () -> job worker) in
    let%bind worker =
      match result with
      | Ok (Ok _) -> return worker
      | Ok (Error _) | Error _ -> recreate worker
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

let destroy {pool = reader, writer; threads; destroy; _} =
  let elements = List.init threads ~f:ignore in
  let%bind () =
    Deferred.List.iter elements ~how:`Parallel ~f:(fun () ->
        match%bind Pipe.read reader with
        | `Eof -> return ()
        | `Ok {thread; state} -> In_thread.run ~thread (fun () -> destroy state))
  in
  return @@ Pipe.close writer
