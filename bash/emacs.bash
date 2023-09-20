#!/usr/bin/env bash

jgdebug "Setting emacs data"

# LOCS
EMAIN_DIR="$HOME/github/_libs/lisp/doom_main"
ENAT_DIR="$HOME/github/_libs/lisp/doom_native"
E30_DIR="$HOME/github/_libs/lisp/emacs30"
BLOOD_DIR="$HOME/github/lisp/blood"

EMAIN_BIN="/usr/local/Cellar/emacs/28.2/bin/emacs"
ENAT_BIN="/usr/local/Cellar/emacs-plus@28/28.2/bin/emacs"

TERM="xterm-24bits"
EDITOR="vim"

EMACS="$ENAT_BIN"
EMACSDIR="$ENAT_DIR"
DOOMDIR="$HOME/.doom.d"

if [[ -n $INSIDE_EMACS ]]
then
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
        "main")
            echo "Setting Main Emacs"
            EMACS="$EMAIN_BIN"
            EMACSDIR="$EMAIN_DIR"
        ;;
        "doom_native")
            echo "Setting Doom Native Emacs"
            EMACS="$ENAT_BIN"
            EMACSDIR="$ENAT_DIR"
            DOOMDIR="$HOME/.doom.d"
        ;;
        "native")
            echo "Setting Native Emacs"
            EMACS="$ENAT_BIN"
            EMACSDIR="$ENAT_DIR"
            DOOMDIR="$HOME/.doom.d"
        ;;
        "30")
            echo "setting Emacs30"
            EMACS="$E30_BIN"
            EMACSDIR="$E30_DIR"
        ;;
        "blood")
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

    if [[ (-L "$HOME/.emacs.d")
            && ($(readlink -f "$HOME/.emacs.d") != $(readlink -f "$EMACSDIR"))
        ]]; then
        rm "$HOME/.emacs.d"
        ln -s "$EMACSDIR" "$HOME/.emacs.d"
    fi
    PATH="$EMACSDIR/bin/:$PATH"
    # alias emacs="$EMACS -nw"
    # alias emacsw="$EMACS"
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
