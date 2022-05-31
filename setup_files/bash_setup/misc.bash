#!/usr/bin/env bash

alias cpu="top -l1 -n0"
alias browse="python -mwebbrowser"
alias lsa="ls -lha"
alias lsd="gls -la --group-directories-first"
alias lsda="ls -la | grep '^d'"
alias ql="qlmanage -p 2>/dev/null"
alias cd="mycd $1"
alias cdc="mycdc $1"
alias cda="mycda $1"
alias pse="ps -A | grep emacs"
alias jgf="find . -maxdepth 1 -iname $1"
#disk usage:
alias duh="du -hd 1 | sort"
#Disable osx gatekeeper: (needs sudo)
alias gatekeeper="spctl --master-disable"
# pdf creator
alias txt2pdf="cupsfilter"

function mycd(){
    builtin cd "$@" && ls
}

function mycdc(){
    builtin cd "$@" && lsc
}

function mycda(){
    builtin cd "$@" && lsa
}


#setting up the prompt:
PS1='(\j): \D{%D} \A \u:  '

#Shell Location update
SHELL="$(which bash)"
