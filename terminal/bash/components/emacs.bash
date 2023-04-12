#!/usr/bin/env bash

# LOCS
EMAIN_DIR="$HOME/github/_libs/lisp/doom_main"
ENAT_DIR="$HOME/github/_libs/lisp/doom_native"
E30_DIR="$HOME/github/_libs/lisp/emacs30"

EMAIN_BIN="/usr/local/Cellar/emacs/28.2/bin/emacs"
ENAT_BIN="/usr/local/Cellar/emacs-plus@28/28.2/bin/emacs"


function check-emacs-d () {
    jgdebug "Setting Emacs-D"
    if [[ ! -e "$HOME/.emacs.d" ]]; then
	ln -s "$EMACSDIR" "$HOME/.emacs.d"
    fi

    if [[ (-L "$HOME/.emacs.d")
            && ($(readlink -f "$HOME/.emacs.d") != $(readlink -f "$EMACSDIR"))
        ]]; then
        rm "$HOME/.emacs.d"
        ln -s "$EMACSDIR" "$HOME/.emacs.d"
    fi
}

function set-emacs () {
    DOOMDIR="$HOME/.doom.d"
    case "$1" in
        "main")
	    echo "Setting Main Emacs"
            EMACS="$EMAIN_BIN"
            EMACSDIR="$EMAIN_DIR"
        ;;
        "native")
	    echo "Setting Native Emacs"
            EMACS="$ENAT_BIN"
            EMACSDIR="$ENAT_DIR"
        ;;
        "30")
	    echo "setting Emacs30"
            EMACS="$E30_BIN"
            EMACSDIR="$E30_DIR"
            DOOMDIR=""
        ;;
        *)
            echo "Unrecognized emacs type"
	    EMACS="$E30_BIN"
            EMACSDIR="$E30_DIR"
            DOOMDIR=""
        ;;
        esac
    check-emacs-d
    PATH="$EMACSDIR/bin/:$PATH"
    alias emacs="$EMACS -nw"
    alias emacsw="$EMACS"
}

function read-emacs () {
    local curr_emacs="$(basename $(readlink -f $HOME/.emacs.d))"
    case "$curr_emacs" in
        "doom_main")
            echo "currently doom main"
            set-emacs "main"
        ;;
        "doom_native")
            echo "currently doom native"
            set-emacs "native"
        ;;
        "emacs")
            echo "currently emacs30"
            set-emacs "30"
        ;;
        *)
            echo "unknown $curr_emacs"
            set-emacs "30"
        ;;
    esac
}

function report-emacs () {
    echo "Emacs       : $EMACS"
    echo ".emacs.d    : $EMACSDIR"
    echo " .doom.d    : $DOOMDIR"
}
