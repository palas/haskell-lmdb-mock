Reasons for the fork
====================

Here we expose the reasons for the fork on `haskell-lmdb`.

New exported functions
----------------------

As we need to be able to resize the environment in order to not pre-allocate too much space
and to be able to grow the database afterwards, we make use of the `mdb_env_set_mapsize`
function so we needed to expose it. Please refer to the documentation of that function to
check its behavior.

We also need to close and reopen the database handlers so we needed to expose the
`mdb_dbi_close'` function. Once again, check the documentation for more information.

Writer mutex changes
--------------------

A bug in `lmbd-simple` can cause a write transaction to be aborted twice when a put fails.
This can lead to a segmentation fault on the next write transaction, because LMDB internally
reuses a single write transaction, yet the transaction is freed if it is aborted/committed
twice. However, `haskell-lmdb` will deadlock if the transaction is aborted or committed
twice, since it uses an MVar mutex to prevent concurrent writes.

To make things a little bit safer, we now check that the thread holds the MVar mutex before
committing or aborting a write transaction. Thus, an exception is thrown before a double
commit/abort can happen.
