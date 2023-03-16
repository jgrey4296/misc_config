#!/usr/bin/env bash

jgdebug "Setting Latex"
jgdebug "reminder: tlmgr for installing packages, use --usermode after tlmgr init-usertree"

# symlink to /usr/local/texlive/2022basic/bin/universal-darwin
PATH="/Library/TeX/texbin:$PATH"

#TEX:
#TEXINPUTS=/Volumes/DOCUMENTS/Dropbox/Scripts/tex/:$TEXINPUTS


TEXMFCNF="$HOME/.doom.d/terminal/tool_configs:"

# MANAPATH="/Library/TeX/Distributions/.DefaultTeX/Contents/Man:$MANPATH"
MANPATH="/Library/TeX/Distributions/.DefaultTeX/Contents/Man:$MANPATH"
