#!/usr/bin/env bash
# https://doc.rust-lang.org/cargo/reference/environment-variables.html
#
jgdebug Setting up rust
RUSTUP_HOME="$JG_CACHE/rustup"
CARGO_HOME="$JG_CACHE/cargo"

PATH="$CARGO_HOME/bin:$PATH"

CARGO_BUILD_JOBS=1

. "$CARGO_HOME/env"
