#!/usr/bin/env bash

jgdebug "Setting emacs data"
BLOOD_DIR="$HOME/github/lisp/blood"
EDITOR="vim"

EMACS="$(which emacs)"
EMACSDIR="$HOME/.emacs.d"
DOOMDIR="$HOME/.config/jg/"

# LOCS
case "$OSTYPE" in
    darwin*)
        ENAT_DIR="$HOME/github/_libs/lisp/doomemacs"
        ENAT_BIN="/usr/local/Cellar/emacs-plus@28/28.2/bin/emacs"
        ;;

    linux*)
        ENAT_DIR="/media/john/data/github/_libs/lisp/doomemacs"
        ENAT_BIN="/snap/bin/emacs"
        ;;
esac

if [[ -n "$INSIDE_EMACS" ]]; then
    echo "Inside Emacs"
    set disable-completion on
    TERM=dumb
fi

function check-emacs-d () {
    if [[ ! -e "$HOME/.emacs.d" ]]; then
        echo "ERROR: No emacs.d"
        # ln -s "$EMACSDIR" "$HOME/.emacs.d"
    fi

    if [[ (-L "$HOME/.emacs.d")
            && ($(readlink -f "$HOME/.emacs.d") != $(readlink -f "$EMACSDIR"))
        ]]; then
        echo "ERROR: emacs.d does not match EMACSDIR"
        rm "$HOME/.emacs.d"
        ln -s "$EMACSDIR" "$HOME/.emacs.d"
    fi
}

function set-emacs () {
    DOOMDIR=""
    case "$1" in
        *native* | doom*)
            echo "Setting Doom Native Emacs"
            EMACS="$ENAT_BIN"
            EMACSDIR="$ENAT_DIR"
            DOOMDIR="$HOME/.config/jg/"
            ;;
        "blood")
            echo "BLOOD"
            EMACS="$ENAT_BIN"
            EMACSDIR="$BLOOD_DIR"
            # To become $BLOODDIR
            DOOMDIR="$EMACSDIR/example"
            ;;
        *)
            echo "Unrecognized emacs type: $1"
            EMACS="$NAT_BIN"
            EMACSDIR="$ENAT_DIR"
        ;;
        esac

    if [[ -d "$HOME/.emacs.d" ]]; then
        echo "Emacs Dir isn't a symlink"
    elif [[ ! ( -L "$HOME/.emacs.d" ) ]]; then
        ln -s "$EMACSDIR" "$HOME/.emacs.d"
    elif [[ (-L "$HOME/.emacs.d")
            && ($(readlink -f "$HOME/.emacs.d") != $(readlink -f "$EMACSDIR"))
        ]]; then
        rm "$HOME/.emacs.d"
        ln -s "$EMACSDIR" "$HOME/.emacs.d"
    fi
    PATH="$EMACSDIR/bin/:$PATH"
}

function read-emacs () {
    local curr_emacs="$(basename $(readlink -f $HOME/.emacs.d))"
    echo "Emacs: $curr_emacs"
}

function report-emacs () {
    echo "Emacs       : $EMACS"
    echo ".emacs.d    : $EMACSDIR"
    echo " .doom.d    : $DOOMDIR"
}
