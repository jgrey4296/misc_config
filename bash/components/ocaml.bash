#!/usr/bin/env bash

jgdebug "Ocaml Setup"


OPAM_SWITCH_PREFIX="$BASE_CACHE/opam/default"
CAML_LD_LIBRARY_PATH="$BASE_CACHE/opam/default/lib/stublibs"
OCAML_TOPLEVEL_PATH="$BASE_CACHE/opam/default/lib/toplevel"

PATH="$BASE_CACHE/opam/default/bin":$PATH
