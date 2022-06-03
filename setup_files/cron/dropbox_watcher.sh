#!/usr/bin/env bash
set -euo pipefail

conda_activate_for_scripts bookmark

WATCH=( "$HOME/Dropbox/docs" "$HOME/Downloads" )
TARGET="$HOME/Desktop/pdfs/Current/"

for Dir in ${WATCH[@]}
do
    echo "Dir: " $Dir
    cd $Dir
    pwd
    mv -u -n *.pdf $TARGET 2> /dev/null || echo "No pdfs to move"
    mv -u -n *.epub $TARGET 2> /dev/null || echo "No Epubs to move"

done

bkmk-bot-stub
