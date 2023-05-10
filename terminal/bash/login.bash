#!/usr/bin/env bash
echo "Login"

source "$HOME/.doom.d/terminal/bash/_basic_utils.bash"
source "$HOME/.doom.d/terminal/bash/_base_path.bash"
source "$HOME/.doom.d/terminal/bash/_aliases.bash"

# Activate components
for fname in $(find "$HOME/.doom.d/terminal/bash/components" -type f -name "*.bash" -not -regex ".+?/_.+?\.bash")
do
    jgdebug "-- Sourcing: $fname"
    source "$fname"
done

# Setup Conda
source "$HOME/.doom.d/terminal/bash/conda.bash"

BASH_ENV="$HOME/.doom.d/terminal/bash/non_interactive.bash"
source "$HOME/.doom.d/terminal/bash/emacs.bash"
source "$HOME/.doom.d/terminal/bash/_exports.bash"

echo "CWD   : $(pwd)"
echo "Date  : $(date)"
jgdebug "Path  : $PATH"

read-emacs
jg_maybe_inc_prompt
jg_set_prompt

if [[ "$TERM_PROGRAM" != "tmux" ]]; then
    tmux new-session -d "nu" "--config" "~/config.nu" "--env-config" "~/env.nu"
    tmux new-window -d -n "emacs" "emacs -nw"
    tmux -2 attach
fi
