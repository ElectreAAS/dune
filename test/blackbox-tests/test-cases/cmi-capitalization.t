When a .ml filename starts with capital letters, we put it into lowercase
in .cmi files.
This means to use this package you'd write 'open Glambda' instead of the expected
'open GLambda'.

  $ cat > dune-project <<EOF
  > (lang dune 3.0)
  > (package
  >  (name glambda))
  > EOF

  $ cat > dune << EOF
  > (library
  >  (name glambda)
  >  (public_name glambda))
  > EOF

  $ cat > GLambda.ml << EOF
  > let f x y = x + y
  > EOF

  $ cat > GLambda.mli << EOF
  > val f : int -> int -> int
  > EOF

  $ dune build @install
  $ cd _build/install/default/lib/glambda

What we have
  $ find glambda.cmi
  glambda.cmi

What we'd want
  $ find GLambda.cmi
  find: 'GLambda.cmi': No such file or directory
  [1]
