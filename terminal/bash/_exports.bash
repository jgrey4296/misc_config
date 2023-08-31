#!/usr/bin/env bash

jgdebug "Setting Exports"

export JG_CACHE="$HOME/.cache"
export HISTFILE="$JG_CACHE/bash_history"
export LESSHISTFILE="$JG_CACHE/lesshst"
export NODE_REPL_HISTORY="$JG_CACHE/node_repl_history"
export PATH
export EDITOR
export PROMPT_NUM
export ANDROID_HOME
export ANDROID_USER_HOME
export ADB_VENDOR_KEYS
export JDK_HOME
export JAVA_HOME
export TERM
export EMACS
export EMACSDIR
export DOOMDIR
export JGLOGDIR
export CMAKE_BUILD_PARALLEL_LEVEL
export GPG_TTY
export JACAMO_HOME
export MANPATH
export BASH_ENV
export GTAGSLABEL
export ANACONDA_HOME
export DYLD_FALLBACK_LIBRARY_PATH
export PYTHONSTARTUP
export CABAL_CONFIG
export CABAL_DIR
export CARGO_HOME
export OPAM_SWITCH_PREFIX
export CAML_LD_LIBRARY_PATH
export OCAML_TOPLEVEL_PATH
export GRADLE_USER_HOME
export RUSTUP_HOME
export CARGO_HOME
export JENV_SHELL
export JENV_LOADED

export BASH_SILENCE_DEPRECATION_WARNING=1
export HOMEBREW_MAKE_JOBS=1

# export TMPDIR

export -f jgdebug
export -f jg_maybe_inc_prompt
export -f jg_prompt_update
export -f jg_set_prompt
# export -f jenv
