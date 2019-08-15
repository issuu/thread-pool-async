open Async_kernel

(** A thread pool *)
type 'state t

(** A single worker thread within the pool *)
type 'state worker

(** Raised if the pool is initiated with a non-positive thread count *)
exception Invalid_thread_count of int

(** Raised if the pool is used after it has been destroyed *)
exception Pool_already_destroyed

(** Initializes a pool. The same name is used for all the threads in the
    pool. Each worker thread also has an associated piece of state. The
    `create` function initializes the state while the `destroy` function
    cleans it up. *)
val init
  :  name:string ->
  threads:int ->
  create:(unit -> 'state) ->
  destroy:('state -> unit) ->
  'state t Deferred.t

(** Runs a piece of work on the first available thread in the pool.

    If the supplied function returns an error, then `retries` specifies the
    maximum number of times that the function is re-run. All retries are
    guaranteed to be on the same thread. If retries are exhausted without a
    successful execution, then the last encountered error is returned.

    If the supplied function raises an exception, then that exception will be
    raised to the monitor that called `with'`.

    Whenever the supplied function returns an error or throws an exception,
    the state associated with the executing thread is cleaned up and a new
    piece of state is created to replace it. *)
val with'
  :  'state t ->
  ?retries:int ->
  ('state -> ('value, 'error) result) ->
  ('value, 'error) Deferred.Result.t

(** Reserves a worker thread and runs a function that can use the Async
    scheduler as well as the worker thread to do its job.

    If the supplied function results in an error, then `retries` specifies
    the maximum number of times that the function is re-run. All retries are
    guaranteed to be on the same thread. If retries are exhausted without a
    successful execution, then the last encountered error is returned.

    If the supplied function raises an exception, then that exception will be
    raised to the monitor that called `with_worker`.

    Whenever the supplied function results in an error or throws an
    exception, the state associated with the executing thread is cleaned up
    and a new piece of state is created to replace it. *)
val with_worker
  :  'state t ->
  ?retries:int ->
  ('state worker -> ('value, 'error) Deferred.Result.t) ->
  ('value, 'error) Deferred.Result.t

(** Executes the supplied function inside the specified worker thread.

    If the supplied function raises an exception, then that exception will be
    raised to the monitor that called `execute`.

    Note that the state associated with the executing worker thread is never
    cleaned up or replaced by this function. *)
val execute : 'state worker -> ('state -> 'outcome) -> 'outcome Deferred.t

(** Frees up all worker threads in the pool as well as the state associated
    with them. *)
val destroy : 'state t -> unit Deferred.t
