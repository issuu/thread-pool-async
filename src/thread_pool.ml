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
    In_thread.Helper_thread.create ~name () >>= fun thread ->
    In_thread.run ~thread create >>= fun state ->
    return (thread, state)
  in
  (* No strict ordering on the heap. All items are equal *)
  let (reader, writer) = Pipe.create () in
  let elements = List.init threads ~f:ignore in
  let add_to_pool () =
    create_thread ~name ~create >>| Pipe.write_without_pushback writer in
  Deferred.List.iter elements ~how:`Parallel ~f:add_to_pool >>= fun () ->
  return { pool = (reader, writer); create; destroy; threads }

let with_ { pool = (reader, writer); create; destroy; _ } ~(f: 'state -> 'result) : 'result Deferred.Or_error.t =
  Pipe.read reader >>= fun read ->
  match read with
  | `Eof -> return (Or_error.errorf "Pool has been destroyed")
  | `Ok (thread, state) ->
      In_thread.run ~thread (fun () -> Or_error.try_with (fun () -> f state)) >>= fun result ->
      (match result with
      | Result.Ok _ -> return state
      | Result.Error _ ->
          In_thread.run ~thread (fun () -> destroy state) >>= fun () ->
          In_thread.run ~thread create >>= fun state ->
          return state)
        >>= fun state' ->
      Pipe.write_without_pushback writer (thread, state');
      return result

let destroy { pool = (reader, writer); threads; destroy; _ } : unit Deferred.t =
  let elements = List.init threads ~f:ignore in
  Deferred.List.iter elements ~how:`Parallel ~f:(fun () ->
    Pipe.read reader >>= fun read ->
    match read with
    | `Eof -> return ()
    | `Ok (thread, state) -> In_thread.run ~thread (fun () -> destroy state))
  >>= fun () ->
  return @@ Pipe.close writer
