#!/usr/bin/env bash

alias emacs="TERM=xterm-24bits emacs -nw"
alias jges="emacs ~/.shell_files/"
alias em="emacs"
alias temacs="emacs ~/github/writing/TODO.org"
alias wemacs="emacs ~/github/writing/main.org"

alias emacs2="/usr/local/Cellar/emacs/28.1/bin/emacs-28.1"

#sclang for emacs:
alias sclangel="sclang -d ~/github/.super_collider_classes/ -r -s -i emacs"

# Doom
PATH=~/.emacs.d/bin/:$PATH

#for gtags:
GTAGSCONF=~/.shell_files/gtags.conf
GTAGSLABEL=pygments

#Default editor:
EDITOR="vim"
