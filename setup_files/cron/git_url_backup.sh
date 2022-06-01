#!/usr/bin/env bash
set -euo pipefail

WATCH=( "$HOME/github" "$HOME/github/otherLibs" "$HOME/github/inform" "$HOME/github/python" "$HOME/github/rust" )
TARGET="$HOME/github/writing/resources/cron_reports/github_urls"
DROPBOX="$HOME/Dropbox/backups"

echo "" > $TARGET
for Dir in $WATCH/*
do
    if [ -d $Dir ]
    then
        cd $Dir
        if git rev-parse --is-inside-work-tree > /dev/null 2> /dev/null
        then
            echo $Dir | sed 's/^.*github/---- github/' >> $TARGET
            Result=$( git config --local -l | awk '/url/ {print $0}' ) || "nothing"
            echo $Result >> $TARGET
            echo "" >> $TARGET
        else
            echo "---- $Dir" >> $TARGET
            echo "Not a repo" >> $TARGET
            echo "" >> $TARGET
        fi
    fi
done

cp $TARGET $DROPBOX
