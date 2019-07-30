open Async_kernel

type 'state t

type 'result computation =
  [ `Ok of 'result
  | `Attempt_retry ]

val init
  :  name:string ->
  threads:int ->
  create:(unit -> 'state) ->
  destroy:('state -> unit) ->
  'state t Deferred.t

val with'
  :  'state t ->
  ?retries:int ->
  ('state -> 'result computation) ->
  'result Deferred.Or_error.t

val destroy : 'state t -> unit Deferred.t
