Thread_pool
===========

Creates an async thread pool so your blocking calls are contained in threads.

Dependencies
------------

  * OPAM 2
  * `topkg-care` (for releases)
  * `topkg-jbuilder` (for releases)

Usage
-----

It is a normal `jbuilder` project, can be built in all normal ways `jbuilder`
software can be built. The `make help` command provides a list of possibly
useful actions.

Release process
---------------

Releasing a new version is handled by `topkg`, check the [Mirage instructions][mirage]
on their packaging process.

1. Write changelog entry
1. `make tag` to tag the repository locally
1. `make distrib` to create a release tarball
1. `make publish` to push the tag and create a GitHub release, upload the tarball
   and set the changelog.

The publish step requires access to a [personal API token][ghapi] on GitHub
that has the `repo` scope.  Possibly only `public_repo` but it is a bit
unclear.

[mirage]: https://mirage.io/wiki/packaging
[ghapi]: https://github.com/settings/tokens
