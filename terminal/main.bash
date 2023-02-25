#!/usr/bin/env Bash

# Login Shell : "bash -" | "bash --login"
# Interactive Shell: "bash" | "bash -s" | "bash -i" , with stdin and stderr connected to terminals

# Interactive login reads: /etc/profile -> ~/.bash_profile -> ~/.bash_login -> ~/.profile
# Interactive, non-login reads: ~/.bashrc
# Non-Interactive reads: $BASH_ENV

# sh reads: /etc/profile -> ~/.profile

# reminder: source this file in $HOME/.bash_profile
# and source .bash_profile in .bashrc
export BASH_SILENCE_DEPRECATION_WARNING=1

source "$HOME/.doom.d/terminal/bash_setup/_basic_utils.bash"
source "$HOME/.doom.d/terminal/bash_setup/_base_path.bash"

if [[ "$OSTYPE" =~ "darwin" ]]; then
    jgd Activating Darwin
    # This is how to activate conda if it
    # complains the shell isn't set up:
    # source $HOME/anaconda/etc/profile.d/conda.sh
    #
    for fname in $(find "$HOME/.doom.d/terminal/bash_setup" -type f -name "*.bash" -not -regex ".+?/_.+?\.bash")
    do
        jgd "-- Sourcing: " "$fname"
        source "$fname"
    done
    source "$HOME/.doom.d/terminal/bash_setup/_exports.bash"
    # Auto Activate an environment if necessary:
    if [ -a ".venv" ]
    then
        ENV=$(tail -n 1 .venv)
        echo "Conda : ${ENV}"
        conda activate "$ENV"
    else
        echo "Conda : ${OSTYPE}"
        conda activate "$CONDA_DEFAULT_ENV"
    fi

fi

echo "CWD   :" `pwd`
echo "Date  :" `date`
jgd  "Path  :" "$PATH"

if [[ -n $INSIDE_EMACS ]]
then
    jgd "Inside Emacs"
    TERM=dumb
else
    read-emacs
fi
