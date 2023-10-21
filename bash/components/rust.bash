#!/usr/bin/env bash
# https://doc.rust-lang.org/cargo/reference/environment-variables.html
#
jgdebug Setting up rust
RUSTUP_HOME="${BASE_CACHE}/rustup"
CARGO_HOME="${BASE_CACHE}/cargo"

PATH="${CARGO_HOME}/bin:$PATH"

CARGO_BUILD_JOBS=1

if [[ -e "${CARGO_HOME}/env" ]]; then
    source "$CARGO_HOME/env"
fi
