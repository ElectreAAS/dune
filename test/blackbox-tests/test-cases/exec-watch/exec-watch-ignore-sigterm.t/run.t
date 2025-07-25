Test exec --watch with a program that ignores sigterm.

  $ dune exec --watch ./foo.exe &
  1: before
  2: before
  $ PID=$!

  $ ../wait-for-file.sh _build/done_flag

  $ sed -i -e 's/1: before/2: before/' foo.ml

  $ ../wait-for-file.sh _build/done_flag

Prevent the test from leaking the dune process.
  $ kill $PID
  $ wait $PID

