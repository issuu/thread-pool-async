0.13.1
======

* Fixed bug that was causing the pool to get stuck (hang) while it was
  destroying itself. This bug could easily be triggered when state creation or
  destruction failed.

0.13.0
======

* Destroy the whole pool in case an exception is thrown while creating or
  destroying a piece of state associated with a worker thread. Indicate this
  through a new exception type.
* Generalize interface to make it possible to use the Async scheduler to
  dynamically determine several related pieces of work that need to run on
  the same thread.
* Stop wrapping exceptions thrown inside a worker thread in an `Or_error`.
  Rethrow them instead.
* Remove `computation` type in favor of the standard `result`.
* Add `.mli` file.

0.12.0
======

* Remove `Deferred.t` from function running in the pool. Running async
  operations outside the main thread is not safe.

0.11.0
======

* Rename module from `Thread_pool` to `Thread_pool_async`
* Use OPAM 2 format
* Do not version constrain Janestreet dependencies

0.10.1
======

* Fix state updates

0.10.0
======

* Rename `Thread_pool.with_` to `Thread_pool.with'`
* Support retries with `Thread_pool.with'`
* Migrate jbuilder to dune

0.9.0
=====

* Initial extraction into a library
* Depend on minimal set of reasonable libraries
* Convert from OUnit2 to alcotest
