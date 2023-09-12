#!/usr/bin/env bash

jgdebug "Ocaml Setup"


OPAM_SWITCH_PREFIX="$JG_CACHE/opam/default"
CAML_LD_LIBRARY_PATH="$JG_CACHE/opam/default/lib/stublibs"
OCAML_TOPLEVEL_PATH="$JG_CACHE/opam/default/lib/toplevel"

PATH="$JG_CACHE/opam/default/bin":$PATH
