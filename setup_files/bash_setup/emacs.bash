#!/usr/bin/env Bash

jgd Setting emacs data
TERM="xterm-24bits"

#sclang for emacs:
alias sclangel="sclang -d ~/github/.super_collider_classes/ -r -s -i emacs"

jgd Setting Doom Emacs data

jgd Setting Gtags Data
GTAGSCONF=$HOME/.shell_files/gtags.conf
GTAGSLABEL=pygments

#Default editor:
EDITOR="vim"

function set-emacs30 () {
    echo "Setting Emacs 30 no doom"
    EMACS="/usr/local/Cellar/emacs-plus@29/29.0.50/bin/emacs"
    EMACSDIR="$HOME/github/otherLibs/lisp/emacs30"
    PATH=$EMACDIR/bin/:$PATH
    alias emacs="$EMACS -nw"
    alias emacsw="$EMACS"
    if [[ -L "$HOME/.emacs.d" ]]; then
        echo "Found .emacs.d link, retargeting"
        rm $HOME/.emacs.d
        ln -s $EMACSDIR $HOME/.emacs.d
    else
        echo "Not a link"
    fi
}

function set-emacs-native () {
    echo "Setting Emacs 28 Native"
    EMACS="/usr/local/Cellar/emacs-plus@28/28.2/bin/emacs"
    EMACSDIR="$HOME/github/otherLibs/lisp/doom_native"
    DOOMDIR="$HOME/.doom.d"
    PATH=$EMACSDIR/bin/:$PATH
    alias emacs="$EMACS -nw"
    alias emacsw="$EMACS"
    if [[ -L "$HOME/.emacs.d" ]]; then
        echo "Found .emacs.d link, retargeting"
        rm $HOME/.emacs.d
        ln -s $EMACSDIR $HOME/.emacs.d
    else
        echo "Not a link"
    fi
}

function set-emacs () {
    echo "Setting Base Emacs"
    EMACS="/usr/local/Cellar/emacs/28.2/bin/emacs"
    EMACSDIR="/Volumes/documents/github/otherLibs/lisp/doom_main"
    DOOMDIR="$HOME/.doom.d"
    PATH=$EMACSDIR/bin/:$PATH
    alias emacs="$EMACS -nw"
    alias emacsw="$EMACS"
    if [[ -L "$HOME/.emacs.d" ]]; then
        echo "Found .emacs.d link, retargeting"
        rm $HOME/.emacs.d
        ln -s $EMACSDIR $HOME/.emacs.d
    else
        echo "Not a link"
    fi
}

function report-emacs () {
    echo "Emacs is    : $EMACS"
    echo "Emacs Dir is: $EMACSDIR"
    echo "User  Dir is: $DOOMDIR"
}
