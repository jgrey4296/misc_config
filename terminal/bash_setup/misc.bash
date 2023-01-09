#!/usr/bin/env bash

jgd Setting misc aliases
# alias tar="gtar"

alias grep="ggrep"
alias sed="gsed"

alias browse="python -mwebbrowser"
alias clingo="clingo -W all"

# top with some nice defaults
alias cpu="top -l1 -n0"

# List active emacs
alias pse="ps -A -O nice | grep -i -E '[0-9]+\.[0-9]+ [^ ]*emacs'"
alias psp="ps -A | grep -i -E 'python' | grep -iv dropbox"

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
