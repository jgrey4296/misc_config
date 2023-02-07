#!/usr/bin/env bash

jgd Setting Cron utils
GIT_BKUP_WATCH=( "$HOME/github" "$HOME/github/otherLibs" "$HOME/github/inform" "$HOME/github/python" "$HOME/github/rust" )
GIT_BKUP_TARGET="$HOME/github/writing/resources/cron_reports/github_urls"
GIT_BKUP_DROPBOX="$HOME/Dropbox/backups"
DROPBOX_WATCH=( "$HOME/Dropbox/docs" "$HOME/Downloads" )
DROPBOX_TARGET="/Volumes/documents/in_progress_pdfs/Current/"
CONDA_MAINTENANCE_TARGET="$HOME/.doom.d/terminal/conda_envs/master_list.yaml"
CPU_MAX="50"
PDF_LIBRARY="$HOME/pdflibrary/"
PDF_SUMMARY="/Volumes/documents/in_progress_pdfs/pdflib_automations/summary/"

function cpu_check(){
    if [[ $# > 1 ]]
    then
        CPU_MAX="$1"
    fi

    if [[ -z "$CPU_MAX" ]]
    then
        CPU_MAX="50"

    fi

    # echo "CPU Max: $CPU_MAX"
    CPU=( $(top -l1 -n0 | awk '/CPU/ {print $0}') )
    UserPerc=( "${CPU[2]}" )
    TESTSTR="${UserPerc[0]/\%/} > ${CPU_MAX}"
    HIGH=$(echo "$TESTSTR" | bc)

    # echo "$TESTSTR : $HIGH"
    if [[ $HIGH -eq 1 ]]
    then
        say -v Moira -r 50 "CPU Usage is higher than ${CPU_MAX} %"
    # else
        # echo "CPU Usage is fine"
    fi
}

export -f cpu_check
