#!/usr/bin/env bash

jgd Setting Cron utils
GIT_BKUP_WATCH=( "$HOME/github" "$HOME/github/otherLibs" "$HOME/github/inform" "$HOME/github/python" "$HOME/github/rust" )
GIT_BKUP_TARGET="$HOME/github/writing/resources/cron_reports/github_urls"
GIT_BKUP_DROPBOX="$HOME/Dropbox/backups"
DROPBOX_WATCH=( "$HOME/Dropbox/docs" "$HOME/Downloads" )
DROPBOX_TARGET="/Volumes/documents/in_progress_pdfs/Current/"
CONDA_MAINTENANCE_TARGET="$HOME/.shell_files/conda_envs/master_list.yaml"
CPU_MAX="50"
PDF_LIBRARY="~/pdflibrary/"
PDF_SUMMARY="/Volumes/documents/in_progress_pdfs/pdflib_automations/summary/"

function conda_maintenance(){
    conda_activate_for_scripts

    conda env export --from-history > $CONDA_MAINTENANCE_TARGET
    echo "--------------------" >> $CONDA_MAINTENANCE_TARGET

    for f in ~/.shell_files/conda_envs/*.yaml; do
        name=`basename -s .yaml $f`
        echo "Found $name"
        if conda_activate_for_scripts $name; then
            echo "-------------------- Loaded $name"
            conda update --all -y
            # conda env export --from-history > $f

            conda env export --from-history  >> $CONDA_MAINTENANCE_TARGET
            echo "--------------------" >> $CONDA_MAINTENANCE_TARGET
            echo "--------------------"
        fi
    done

    conda update conda -y
    conda update --all -y
}

function cpu_check(){
    if [ $# > 1 ]
    then
        CPU_MAX="$1"
    fi

    if [ -z $CPU_MAX ]
    then
        CPU_MAX="50"

    fi

    # echo "CPU Max: $CPU_MAX"
    CPU=( $(top -l1 -n0 | awk '/CPU/ {print $0}') )
    UserPerc=( ${CPU[2]} )
    TESTSTR="${UserPerc[0]/\%/} > ${CPU_MAX}"
    HIGH=$(echo $TESTSTR | bc)

    # echo "$TESTSTR : $HIGH"
    if [ $HIGH -eq 1 ]
    then
        say -v Moira -r 50 "CPU Usage is higher than ${CPU_MAX} %"
    # else
        # echo "CPU Usage is fine"
    fi
}

function dropbox_watcher(){
    conda_activate_for_scripts bookmark

    for Dir in ${DROPBOX_WATCH[@]}
    do
        echo "Dir: " $Dir
        cd $Dir
        pwd
        find . -maxdepth 1 -iregex ".+?*\.\(pdf\|epub\)" -print0 | xargs -0 -I {} mv -u -n {} $DROPBOX_TARGET
        find . -maxdepth 1 -iregex ".+?\.\(pdf\|epub\)" -printf "%f\n" -print0 | xargs -0 -I {} mv {} exists_{}

    done

    echo "Stubbing"
    bkmk-bot-stub
    echo "Stubbing finished"
}

function git_url_backup(){

    echo "" > $GIT_BKUP_TARGET
    for Dir in $GIT_BKUP_WATCH/*
    do
        if [ -d $Dir ]
        then
            cd $Dir
            if git rev-parse --is-inside-work-tree > /dev/null 2> /dev/null
            then
                echo $Dir | sed 's/^.*github/---- github/' >> $GIT_BKUP_TARGET
                Result=$( git config --local -l | awk '/url/ {print $0}' ) || "nothing"
                echo $Result >> $GIT_BKUP_TARGET
                echo "" >> $GIT_BKUP_TARGET
            else
                echo "---- $Dir" >> $GIT_BKUP_TARGET
                echo "Not a repo" >> $GIT_BKUP_TARGET
                echo "" >> $GIT_BKUP_TARGET
            fi
        fi
    done

    cp $GIT_BKUP_TARGET $GIT_BKUP_DROPBOX
}

function run_maintenance(){
    # General maintenance scripts
    # Update node, brew, doom etc
    echo "Conda --------------------"
    conda_maintenance

    echo "Brew Cleanup --------------------"
    brew cleanup
    echo "--------------------"

    echo "Brew Update --------------------"
    brew update
    echo "--------------------"

    echo "Doom Upgrade --------------------"
    doom upgrade
    echo "--------------------"

    echo "CABAL Update--------------------"
    cabal update
    echo "--------------------"

    echo "NPM Update--------------------"
    npm update npm -g
    echo "--------------------"

    echo "Latex Update--------------------"
    latex_summarise
    # tlmgr update --all

    echo "--------------------"
}

function pdf_summarise(){
    conda_activate_for_scripts bookmark

    bkmk-pdf-summarise --target "$PDF_LIBRARY" --output "$PDF_SUMMARY" -r
}

function latex_summarise(){
    tlmgr info --only-installed > "$HOME/.doom.d/setup_files/latex/installed_packages"
}

alias dropwatch="dropbox_watcher"

export -f conda_maintenance
export -f cpu_check
export -f dropbox_watcher
export -f git_url_backup
export -f run_maintenance
