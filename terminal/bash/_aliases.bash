#!/usr/bin/env bash

jgdebug Setting misc aliases
# alias tar="gtar"

alias ghidra="ghidraRun"
alias nsa="ghidraRun"

alias grep="ggrep"
alias sed="gsed"

alias browse="python -mwebbrowser"
alias clingo="clingo -W all"

# top with some nice defaults
alias cpu="top -l1 -n0"

# List active emacs
alias pse="ps -A -O nice | grep -i -E '[0-9]+\.[0-9]+ [^ ]*emacs'"
alias psp="ps -A | grep -i -E 'python' | grep -iv dropbox"

#sclang for emacs:
alias sclangel="sclang -d ~/github/.super_collider_classes/ -r -s -i emacs"

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

alias dbash="env JGDEBUG=true bash"
alias q="exit"

# alias pServer="python -m http.server 8888 &"
# alias qpServer="(python -m http.server 8888 > /dev/null 2>&1) & && (echo 'Running Quiet Server')"
# alias qpoServer="((python -m http.server 8888 > /dev/null 2>&1) &) && (echo 'Running Quiet Server') && (open http://localhost:8888/)"

#for automating the update of pip packages:
# alias pipupdate="pip list --outdated --format=freeze | grep -v '^\-e' | cut -d = -f 1 | xargs -n1 pip install -U"

# conda environments
alias cenv="conda activate"
alias clist="conda env list"
alias cexport="conda env export --from-history"

alias wake="wakeonlan -i 192.168.1.20 90e6ba7391f9"

# Nushell
alias nu="nu --config ~/config.nu --env-config ~/env.nu"
