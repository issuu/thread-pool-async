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
