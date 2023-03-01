#!/usr/bin/env bash
# https://doc.rust-lang.org/cargo/reference/environment-variables.html
#
jgd Setting up rust
RUSTUP_HOME="$HOME/.rustup"
CARGO_HOME="$HOME/.cargo"

PATH="$CARGO_HOME/bin:$PATH"

CARGO_BUILD_JOBS=1

. "$CARGO_HOME/env"
