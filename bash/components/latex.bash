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
       PATH="$HOME/.local/texlive/2023/bin/x86_64-linux:$PATH"
       MANPATH="$HOME/.local/texlive/2023/texmf-dist/doc/man:$MANPATH"
       ;;
esac
