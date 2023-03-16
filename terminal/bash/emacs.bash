#!/usr/bin/env bash

jgdebug "Setting emacs data"
TERM="xterm-24bits"

#Default editor:
EDITOR="vim"

if [[ -n $INSIDE_EMACS ]]
then
    echo "Inside Emacs"
    set disable-completion on
    TERM=dumb
fi
