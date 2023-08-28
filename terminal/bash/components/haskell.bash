#!/usr/bin/env bash

jgdebug "Setting haskell"
#don't forget to update emacs haskell program name variable when modifying this
alias whaskell="ghci -Wall -fwarn-name-shadowing"
alias hs="haskell"

CABAL_CONFIG="$HOME/.cabalrc"
CABAL_DIR="$JG_CACHE/cabal"
