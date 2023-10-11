#!/usr/bin/env bash

if [[ $TMUX ]]; then
	echo "Login: $OSTYPE : $TERM : TMUX" 
else
	echo "Login: $OSTYPE : $TERM"
fi
echo "Date  : $(date).  CWD: $(pwd)"

source "$HOME/.config/jg/bash/_basic_utils.bash"
source "$HOME/.config/jg/bash/_base_path.bash"

case "$OSTYPE" in 
	darwin*) source "$HOME/.config/jg/bash/_aliases.mac.bash"
		 jgdebug "Activating components"
		 for fname in $(find "$HOME/.config/jg/bash/components" -type f -name "*.bash" -not -regex ".+?/_.+?\.bash")
		 do
		     jgdebug "-- Sourcing: $fname"
		     source "$fname"
		 done 
		 jgdebug "Setting up Conda"
		 source "$HOME/.config/jg/bash/conda.bash" ;;
	linux*) source "$HOME/.config/jg/bash/_aliases.linux.bash"
		 for fname in $(find "$HOME/.config/jg/bash/components" -type f -name "*.bash" -not -regex ".+?/_.+?\.bash")
		 do
		     jgdebug "-- Sourcing: $fname"
		     source "$fname"
		 done
        source "$JG_CONFIG/bash/conda.bash"
        ;;
esac

BASH_ENV="$HOME/.config/jg/bash/non_interactive.bash"
source "$HOME/.config/jg/bash/emacs.bash"
source "$HOME/.config/jg/bash/_exports.bash"

jgdebug "Path  : $PATH"

read-emacs
jg_maybe_inc_prompt
jg_set_prompt

loginmux
