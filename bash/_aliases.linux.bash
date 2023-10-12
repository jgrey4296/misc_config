#!/usr/bin/env bash

jgdebug Setting misc aliases
# alias tar="gtar"

alias fd="fdfind"
alias browse="python -mwebbrowser"
alias clingo="clingo -W all"
alias cpu="btop"
alias dbash="env JGDEBUG=true bash"
alias q="exit"

# List active emacs
alias pse="ps -A -O nice | grep -i -E '[0-9]+\.[0-9]+ [^ ]*emacs'"
alias psp="ps -A | grep -i -E 'python' | grep -iv dropbox"

# ls shortcuts
alias lsa="ls -lha"
alias lsd="ls -d */"
alias lsda="ls -la | grep '^d'"
alias lsl="ls -l"

# enable color support of ls and also add handy aliases
if [[ -x /usr/bin/dircolors ]] && [[ "$TERM" != "dumb" ]]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    alias dir='dir --color=auto'
    alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

#disk usage:
alias duh="du -hd 1 | sort"

# cd + ls
alias cd="cd_ls"
function cd_ls {
    builtin cd $@
    ls
}

#for automating the update of pip packages:
# alias pipupdate="pip list --outdated --format=freeze | grep -v '^\-e' | cut -d = -f 1 | xargs -n1 pip install -U"

# conda environments
alias cenv="mamba activate"
