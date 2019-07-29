open Base
open Async_kernel
open Async_unix

type 'state worker = {
  thread : In_thread.Helper_thread.t;
  state : 'state;
}

type 'outcome result = 'outcome Deferred.Or_error.t

type 'state t = {
  pool : 'state worker Pipe.Reader.t * 'state worker Pipe.Writer.t;
  create : unit -> 'state;
  destroy : 'state -> unit;
  threads : int;
}

let init ~name ~threads ~create ~destroy =
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

let execute {thread; state} work =
  In_thread.run ~thread (fun () -> Or_error.try_with (fun () -> work state))

let with_worker {pool = reader, writer; destroy; create; _} ?(retries = 0) job =
  let recreate {thread; state} =
    let%bind () = In_thread.run ~thread (fun () -> destroy state) in
    let%bind state = In_thread.run ~thread create in
    return @@ {thread; state}
  in
  let run_once worker =
    match%bind job worker with
    | Ok (Some _) as result -> return (result, worker)
    | (Ok None | Error _) as result ->
        let%bind worker = recreate worker in
        return (result, worker)
  in
  let rec run_with_retry worker retries_left =
    let%bind outcome, worker = run_once worker in
    match outcome with
    | Ok (Some outcome) -> return (Ok outcome, worker)
    | Ok None -> (
      match retries_left with
      | n when n <= 0 -> return (Or_error.error_string "No more retries", worker)
      | n -> run_with_retry worker (Int.pred n))
    | Error _ as error -> return (error, worker)
  in
  match%bind Pipe.read reader with
  | `Eof -> return @@ Or_error.errorf "Pool has been destroyed"
  | `Ok worker ->
      let%bind outcome, worker = run_with_retry worker retries in
      Pipe.write_without_pushback writer worker;
      return outcome

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
