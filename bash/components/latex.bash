#!/usr/bin/env bash

jgdebug "Setting Latex"
jgdebug "reminder: tlmgr for installing packages, use --usermode after tlmgr init-usertree"

case "$OSTYPE" in
    darwin*)
        # symlink to /usr/local/texlive/2022basic/bin/universal-darwin
        PATH="/Library/TeX/texbin:$PATH"
        # MANAPATH="/Library/TeX/Distributions/.DefaultTeX/Contents/Man:$MANPATH"
        MANPATH="/Library/TeX/Distributions/.DefaultTeX/Contents/Man:$MANPATH"
        ;;
   linux*)

       ;;
esac

# TEXMFCNF="$HOME/.config/jg/templates/tex-config/"
