#!/usr/bin/env bash

jgd Setting emacs data
TERM="xterm-24bits"

#Default editor:
EDITOR="vim"

if [[ -n $INSIDE_EMACS ]]
then
    echo "Inside Emacs"
    TERM=dumb
fi
