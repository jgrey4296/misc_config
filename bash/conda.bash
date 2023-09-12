#https://stackoverflow.com/questions/36365801/run-a-crontab-job-using-an-anaconda-env/60977676#60977676

if [[ -z "${ANACONDA_HOME}" ]]; then
    echo "Setting conda home"
    # export ANACONDA_HOME=/usr/local/anaconda3
    export ANACONDA_HOME="/usr/local/Caskroom/mambaforge/base"
fi

if [[ -n "${CONDA_DEFAULT_ENV}" ]]; then
    # echo "Conda defaulting to: ${CONDA_DEFAULT_ENV}"
    :
elif [[ -f ".venv" ]]; then
    ENV=$(tail -n 1 .venv)
    echo "Env Conda : ${ENV}"
else
    echo "Base Conda"
    CONDA_DEFAULT_ENV="base"
fi

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('$ANACONDA_HOME/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
if [[ $? -eq 0 ]]; then
    eval "$__conda_setup"
elif [[ -f "${ANACONDA_HOME}/etc/profile.d/conda.sh" ]]; then
    . "${ANACONDA_HOME}/etc/profile.d/conda.sh"
else
    export PATH="${ANACONDA_HOME}/bin:${PATH}"
fi
unset __conda_setup

if [[ -f "${ANACONDA_HOME}/etc/profile.d/mamba.sh" ]]; then
    . "${ANACONDA_HOME}/etc/profile.d/mamba.sh"
fi
# <<< conda initialize <<<

mamba activate "${CONDA_DEFAULT_ENV}"
