#!/usr/bin/env bash

DROPBOX_WATCH=( "$HOME/Dropbox/docs" "$HOME/Downloads" )
DROPBOX_TARGET="$HOME/Desktop/pdfs/Current/"
CONDA_MAINTENANCE_TARGET="$HOME/.shell_files/conda_envs/base_env.yaml"
CPU_MAX="50"

function conda_maintenance(){
    conda_activate_for_scripts

    conda env export > $CONDA_MAINTENANCE_TARGET

    for f in ~/.shell_files/conda_envs/*.yaml; do
        name=`basename -s .yaml $f`
        echo "Found $name"
        if conda_activate_for_scripts $name; then
            echo "Loaded $name"
            conda update --all -y
            conda env export --from-history > $f
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
        mv -u -n *.pdf $DROPBOX_TARGET 2> /dev/null || echo "No pdfs to move"
        mv -u -n *.epub $DROPBOX_TARGET 2> /dev/null || echo "No Epubs to move"

        find . -maxdepth 1 \( -name "*.pdf" -o -name "*.epub" \) -printf "%f\n" | xargs -I {} mv {} exists_{}

    done

    echo "Stubbing"
    bkmk-bot-stub
    echo "Stubbing finished"
}

GIT_BKUP_WATCH=( "$HOME/github" "$HOME/github/otherLibs" "$HOME/github/inform" "$HOME/github/python" "$HOME/github/rust" )
GIT_BKUP_TARGET="$HOME/github/writing/resources/cron_reports/github_urls"
GIT_BKUP_DROPBOX="$HOME/Dropbox/backups"

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
}


export -f conda_maintenance
export -f cpu_check
export -f dropbox_watcher
export -f git_url_backup
export -f run_maintenance