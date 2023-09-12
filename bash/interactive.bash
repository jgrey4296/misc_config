#!/usr/bin/env bash
# Interactive Shell: "bash" | "bash -s" | "bash -i" ,
# with stdin and stderr connected to terminals

echo "Interactive"

source "$HOME/.doom.d/bash/_aliases.bash"
source "$HOME/.doom.d/bash/emacs.bash"

# Setup Conda
source "$HOME/.doom.d/bash/conda.bash"

jg_maybe_inc_prompt
jg_set_prompt
