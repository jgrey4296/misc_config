# -*- mode: snippet -*-
# name: block.pl
# key: block.pl
# --
#+NAME: $1
#+HEADER: :goal main
#+begin_src prolog :results value
$4
  main :- format("True").
  main :- format("False").
#+end_src
$0
