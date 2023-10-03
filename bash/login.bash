#!/usr/bin/env bash
echo "Login"

source "$HOME/.doom.d/bash/_basic_utils.bash"
source "$HOME/.doom.d/bash/_base_path.bash"
source "$HOME/.doom.d/bash/_aliases.bash"

# Activate components
for fname in $(find "$HOME/.doom.d/bash/components" -type f -name "*.bash" -not -regex ".+?/_.+?\.bash")
do
    jgdebug "-- Sourcing: $fname"
    source "$fname"
done

# Setup Conda
source "$HOME/.doom.d/bash/conda.bash"

BASH_ENV="$HOME/.doom.d/bash/non_interactive.bash"
source "$HOME/.doom.d/bash/emacs.bash"
source "$HOME/.doom.d/bash/_exports.bash"

echo "CWD   : $(pwd)"
echo "Date  : $(date)"
jgdebug "Path  : $PATH"

read-emacs
jg_maybe_inc_prompt
jg_set_prompt

jointmux
