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

# TODO handle venv and condaenv?
# Auto Activate an environment if necessary:
if [ -f ".venv" ]
then
    ENV=$(tail -n 1 .venv)
    echo "Env Conda : ${ENV}"
    conda activate "$ENV"
fi
