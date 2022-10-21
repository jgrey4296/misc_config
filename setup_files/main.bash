# reminder: source this file in ~/.bash_profile
# and source .bash_profile in .bashrc

source ~/.shell_files/bash_setup/_basic_utils.bash
source ~/.shell_files/bash_setup/_base_path.bash

if [[ $OSTYPE =~ "darwin" ]]; then
    jgd Activating Darwin
    # This is how to activate conda if it
    # complains the shell isn't set up:
    # source ~/anaconda3/etc/profile.d/conda.sh
    #
    for fname in $(find ~/.shell_files/bash_setup -type f -name "*.bash" -not -regex ".+?/_.+?\.bash")
    do
        jgd "-- Sourcing: " $fname
        source $fname
    done
    source ~/.shell_files/bash_setup/_exports.bash
    # Auto Activate an environment if necessary:
    if [ -a ".venv" ]
    then
        ENV=$(tail -n 1 .venv)
        echo "Conda : ${ENV}"
        conda activate $ENV
    else
        echo "Conda : ${OSTYPE}"
        conda activate $CONDA_DEFAULT_ENV
    fi

fi

echo "CWD   :" `pwd`
echo "Date  :" `date`
jgd  "Path  :" $PATH
