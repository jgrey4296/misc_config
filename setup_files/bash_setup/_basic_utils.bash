#!/usr/bin/env bash

function jgdebug () {
    if [[ -n $JGDEBUG ]]; then
        echo "%% " $@
    fi
}

alias dbash="env JGDEBUG=true bash"
alias jgd="jgdebug"
alias q="exit"
jgd Debug is $JGDEBUG


# Increment the shell level each time you go into a subshell
if [ -n "$PROMPT_NUM" ] && [ $PROMPT_NUM -eq $PROMPT_NUM ] 2> /dev/null; then
    jgd Prompt Level: $PROMPT_NUM
    PROMPT_NUM=$(($PROMPT_NUM + 1))
else
    PROMPT_NUM=1
fi
jgd Depth Prompt: $DEPTH_PROMPT

#setting up the prompt:
# from https://unix.stackexchange.com/questions/216953
JGCONDA=""
function jgprompt {
    DEPTH_PROMPT=$PROMPT_NUM
    if [ $PROMPT_NUM -lt 2 ]; then
        DEPTH_PROMPT="⟘"
    fi
    JGPATH=$(pwd | sed -r 's/.+?\/(.+?\/.+?)/...\/\1/')

    if [[ -n $CONDA_DEFAULT_ENV ]]; then
        JGCONDA="py:$CONDA_DEFAULT_ENV"
    fi
    }

PROMPT_COMMAND='jgprompt'
# Also modified in .condarc
PS1='  (u:\u j:\j) py:$CONDA_DEFAULT_ENV |- $JGPATH[$DEPTH_PROMPT]: '

#Shell Location update
SHELL="$(which bash)"

# TEMP locations
# TMPDIR="~/.temp"
