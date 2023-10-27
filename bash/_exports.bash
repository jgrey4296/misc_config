#!/usr/bin/env bash

jgdebug "Setting Exports"

export HISTFILE="$BASE_CACHE/logs/bash_history"
export LESSHISTFILE="$BASE_CACHE/logs/lesshst"
export NODE_REPL_HISTORY="$BASE_CACHE/logs/node_repl_history"

# To get rid of atk-bridge complaints:
export G_MESSAGES_PREFIXED=""

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
export SDKMAN_DIR
export GNUPGHOME
export MUHOME
export MAILDIR
export SOAR_HOME
export CLIPS_HOME
export CEPTRE_HOME
export make_jobs
export CMAKE_BUILD_PARALLEL_LEVEL
export DOTNET_ROOT
export NUGET_PACKAGES
export DOTNET_CLI_HOME

export BASH_SILENCE_DEPRECATION_WARNING=1

case "$OSTYPE" in
    darwin*)
        export HOMEBREW_MAKE_JOBS=1
        ;;
    linux*)

        ;;
esac

# export TMPDIR

export -f jgdebug
export -f jg_maybe_inc_prompt
export -f jg_prompt_update
export -f jg_set_prompt
export -f randname
