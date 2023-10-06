#!/usr/bin/env bash

if [[ $TMUX ]]; then
	echo "Login: $OSTYPE : $TERM : TMUX" 
else
	echo "Login: $OSTYPE : $TERM"
fi
echo "Date  : $(date).  CWD: $(pwd)"

source "$HOME/.doom.d/bash/_basic_utils.bash"
source "$HOME/.doom.d/bash/_base_path.bash"

case "$OSTYPE" in 
	darwin*) source "$HOME/.doom.d/bash/_aliases.bash" 
		 jgdebug "Activating components"
		 for fname in $(find "$HOME/.doom.d/bash/components" -type f -name "*.bash" -not -regex ".+?/_.+?\.bash")
		 do
		     jgdebug "-- Sourcing: $fname"
		     source "$fname"
		 done 
		 jgdebug "Setting up Conda"
		 source "$HOME/.doom.d/bash/conda.bash" ;;
	linux*)  
        source "$JG_CONFIG/bash/conda.bash" 
        ;;
esac

BASH_ENV="$HOME/.doom.d/bash/non_interactive.bash"
source "$HOME/.doom.d/bash/emacs.bash"
source "$HOME/.doom.d/bash/_exports.bash"

jgdebug "Path  : $PATH"

read-emacs
jg_maybe_inc_prompt
jg_set_prompt

loginmux
