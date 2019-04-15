open Base
open Async_kernel
open Async_unix

type 'state thread = In_thread.Helper_thread.t * 'state

type 'state t = {
  pool: ('state thread Pipe.Reader.t * 'state thread Pipe.Writer.t);
  create: unit -> 'state;
  destroy: 'state -> unit;
  threads: int;
}

let init ~name ~(threads: int) ~(create: unit -> 'state) ~(destroy: 'state -> unit): 'state t Deferred.t =
  let create_thread ~name ~create =
    let%bind thread = In_thread.Helper_thread.create ~name () in
    let%bind state = In_thread.run ~thread create in
    return (thread, state)
  in
  (* No strict ordering on the heap. All items are equal *)
  let (reader, writer) = Pipe.create () in
  let elements = List.init threads ~f:ignore in
  let add_to_pool () =
    create_thread ~name ~create >>| Pipe.write_without_pushback writer in
  let%bind () = Deferred.List.iter elements ~how:`Parallel ~f:add_to_pool in
  return { pool = (reader, writer); create; destroy; threads }

type 'result computation = [`Ok of 'result | `Attempt_retry]

let with'
  : 'state t -> ?retries:int -> ('state -> 'result computation) -> 'result Deferred.Or_error.t
  = fun { pool = (reader, writer); create; destroy; _ } ?(retries=0) f ->
  assert (retries >= 0);

  let recreate ~thread state =
    let%bind () = In_thread.run ~thread (fun () -> destroy state) in
    In_thread.run ~thread create
  in

  let run_once
    : 'thread -> 'state -> ('result computation Or_error.t * 'state) Deferred.t
    = fun thread state ->
    let%bind result = In_thread.run ~thread (fun () -> Or_error.try_with (fun () -> f state)) in
    match result with
    | Ok `Ok res -> return (Ok (`Ok res), state)
    | Ok `Attempt_retry ->
      let%bind state = recreate ~thread state in
      return (Ok `Attempt_retry, state)
    | Error _ as e ->
      let%bind state = recreate ~thread state in
      return (e, state)
  in
  match%bind Pipe.read reader with
  | `Eof -> return @@ Or_error.errorf "Pool has been destroyed"
  | `Ok (thread, state) ->
    let rec run_with_retry state retries_left =
      let%bind result, state = run_once thread state in
      match result with
      | Ok `Ok result -> return (Ok result, state)
      | Ok `Attempt_retry ->
        (match retries_left with
        | n when n <= 0 -> return (Or_error.error_string "No more retries", state)
        | n -> run_with_retry state (Int.pred n))
      | Error err -> return (Error err, state)
    in
    let%bind result, state' = run_with_retry state retries in
    Pipe.write_without_pushback writer (thread, state');
    return result

let destroy { pool = (reader, writer); threads; destroy; _ } : unit Deferred.t =
  let elements = List.init threads ~f:ignore in
  let%bind () =
    Deferred.List.iter elements ~how:`Parallel ~f:(fun () ->
        match%bind Pipe.read reader with
        | `Eof -> return ()
        | `Ok (thread, state) -> In_thread.run ~thread (fun () -> destroy state)
      )
  in
  return @@ Pipe.close writer
