#!/usr/bin/env bash
echo "Login"

source "$HOME/.doom.d/terminal/bash/_basic_utils.bash"
source "$HOME/.doom.d/terminal/bash/_base_path.bash"
source "$HOME/.doom.d/terminal/bash/_aliases.bash"

# Setup Conda
source "$HOME/.doom.d/terminal/bash/conda.bash"

# Activate components
for fname in $(find "$HOME/.doom.d/terminal/bash/components" -type f -name "*.bash" -not -regex ".+?/_.+?\.bash")
do
    jgd "-- Sourcing: " "$fname"
    source "$fname"
done

BASH_ENV="$HOME/.doom.d/terminal/bash/non_interactive.bash"
source "$HOME/.doom.d/terminal/bash/emacs.bash"
source "$HOME/.doom.d/terminal/bash/_exports.bash"

#Shell Location update
SHELL="$(which bash)"

echo "Login Conda : ${OSTYPE}"
conda activate "$CONDA_DEFAULT_ENV"

echo "CWD   :" `pwd`
echo "Date  :" `date`
jgd  "Path  :" "$PATH"

read-emacs
jg_maybe_inc_prompt
jg_set_prompt

if [[ "$TERM_PROGRAM" != "tmux" ]]; then
    tmux new -d -s "shell" "bash"
    tmux new-window -d -n "emacs" "emacs"
    tmux attach -t "shell"
else
    echo "Already in Tmux"
fi

# emacs
