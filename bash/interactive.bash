#!/usr/bin/env bash
# Interactive Shell: "bash" | "bash -s" | "bash -i" ,
# with stdin and stderr connected to terminals
if [[ $- != *i* ]]; then
	return
fi

echo "Interactive"
source "$HOME/.config/jg/bash/_basic_utils.bash"
source "$HOME/.config/jg/bash/emacs.bash"

case "$OSTYPE" in
	darwin*) source "$HOME/.config/jg/bash/_aliases.bash"
		# Setup Conda
		source "$HOME/.config/jg/bash/conda.bash"
		echo "Stopping Sarafi Bookmarks"; launchctl stop com.apple.SafariBookmarksSyncAgent
		;;
	linux*) 
       source "$HOME/.config/jg/bash/conda.bash"
        ;;
esac

jg_maybe_inc_prompt
jg_set_prompt
