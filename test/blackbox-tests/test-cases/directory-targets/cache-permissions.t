We expect a clean restore when some directory targets have modes like 755.

See #11533.

This test relies on a particular umask.

  $ umask 002

  $ export DUNE_CACHE_ROOT=$PWD/.cache
  $ export DUNE_CACHE=enabled

  $ cat > dune-project <<EOF
  > (lang dune 3.0)
  > (using directory-targets 0.1)
  > EOF

  $ cat > dune <<EOF
  > (rule
  >  (targets (dir d))
  >  (action
  >   (progn
  >    (bash "echo building d")
  >    (run mkdir d)
  >    (run chmod 755 d)
  >    (run touch d/x))))
  > 
  > (rule
  >  (deps d)
  >  (action
  >   (progn
  >    (echo building other)
  >    (write-file other data))))
  > EOF

First build: `d` (with mode 755) and `other` are stored in cache

  $ dune build other
  Before chmod 1a: 444
  chmod 1a went through
  Before chmod 1a: 444
  chmod 1a went through
  building d
  Before chmod 1a: 444
  chmod 1a went through
  building otherBefore chmod 1a: 444
  chmod 1a went through

The chmod command was run, so this is expected
  $ dune_cmd stat permissions _build/default/d
  755

Second build: `d` is restored and `other` can use it, so no rebuild happens.

  $ dune clean
  $ dune build other
  Before chmod 2: 444
  Before chmod 2: 444
  Before chmod 2: 444
  building other

We'll note that the permissions are still set to the umask
  $ dune_cmd stat permissions _build/default/d
  775
