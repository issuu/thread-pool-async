open Async_kernel

(** A thread pool *)
type 'state t

(** A single worker thread within the pool *)
type 'state worker

(** The type returned by functions that schedule work in the pool or on
    specific worker threads *)
type 'outcome result = 'outcome Deferred.Or_error.t

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

    If the supplied function results in `None`, then `retries` specifies the
    maximum number of times that the function is re-run. All retries are
    guaranteed to be on the same thread. If retries are exhausted without a
    successful execution, then an error is returned.

    Whenever the supplied function returns `None` or throws an exception, the
    state associated with the executing thread is cleaned up and a new piece
    of state is created. *)
val with' : 'state t -> ?retries:int -> ('state -> 'outcome option) -> 'outcome result

(** Reserves a worker thread and runs a function that can use the Async
    scheduler as well as the worker thread to do its job.

    If the supplied function binds to `Ok None`, then `retries` specifies the
    maximum number of times that the function is re-run. All retries are
    guaranteed to be on the same thread. If retries are exhausted without a
    successful execution, then an error is returned.

    Whenever the supplied function binds to `Ok None` or to an error, the
    state associated with the executing thread is cleaned up and a new piece
    of state is created. *)
val with_worker
  :  'state t ->
  ?retries:int ->
  ('state worker -> 'outcome option result) ->
  'outcome result

(** Executes the supplied function inside the specified worker thread.

    Whenever the supplied function throws an exception, an error is returned.
    
    Note that the state associated with the executing worker thread is never
    cleaned up or replaced by this function. *)
val execute : 'state worker -> ('state -> 'outcome) -> 'outcome result

(** Frees up all worker threads in the pool as well as the state associated
    with them. *)
val destroy : 'state t -> unit Deferred.t
