#!/usr/bin/env bash

# alias tar="gtar"

alias browse="python -mwebbrowser"
alias clingo="clingo -W all"

# top with some nice defaults
alias cpu="top -l1 -n0"

# List active emacs
alias pse="ps -A | grep -i -E '[0-9]+\.[0-9]+ [^ ]*emacs'"
alias psp="ps -A | grep -i -E '[0-9] python$' | grep -iv dropbox"

# Finder Quick Look from cli
alias ql="qlmanage -p 2>/dev/null"

# ls shortcuts
alias lsa="ls -lha"
alias lsd="ls -d */"
alias lsda="ls -la | grep '^d'"
alias lsl="ls -l"
#disk usage:
alias duh="du -hd 1 | sort"
#Disable osx gatekeeper: (needs sudo)
alias gatekeeper="spctl --master-disable"
# pdf creator
alias txt2pdf="cupsfilter"

# cd + ls
alias cd="cd_ls"
function cd_ls {
    builtin cd $@
    ls
}
alias cdd="cd_ls_dir"
function cd_ls_dir {
    builtin cd $@
    ls -d */
}
alias cda="cd_ls_all"
function cd_ls_all {
    buildin cd $@
    ls -la
}

# Increment the shell level each time you go into a subshell
if [ -n "$PROMPT_NUM" ] && [ $PROMPT_NUM -eq $PROMPT_NUM ] 2> /dev/null; then
    PROMPT_NUM=$(($PROMPT_NUM + 1))
else
    PROMPT_NUM=1
fi
DEPTH_PROMPT=$PROMPT_NUM

if [ $PROMPT_NUM -lt 2 ]; then
    DEPTH_PROMPT="⟘"
fi


#setting up the prompt:
# num of jobs, date, time, user, depth
PS1=' (\j): \D{%D} \A \u ($DEPTH_PROMPT):  '

#Shell Location update
SHELL="$(which bash)"

# TEMP locations
TMPDIR="~/.temp"
