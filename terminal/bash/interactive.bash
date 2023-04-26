#!/usr/bin/env bash
# Interactive Shell: "bash" | "bash -s" | "bash -i" ,
# with stdin and stderr connected to terminals

echo "Interactive"

source "$HOME/.doom.d/terminal/bash/_aliases.bash"
source "$HOME/.doom.d/terminal/bash/emacs.bash"

# Setup Conda
source "$HOME/.doom.d/terminal/bash/conda.bash"

#Shell Location update
SHELL="$(which bash)"

jg_maybe_inc_prompt
jg_set_prompt
