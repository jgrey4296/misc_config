# https://stackoverflow.com/questions/36365801/run-a-crontab-job-using-an-anaconda-env/60977676#60977676


if [[ -n "${CONDA_DEFAULT_ENV}" ]]; then
    # echo "Conda defaulting to: ${CONDA_DEFAULT_ENV}"
    :
elif [[ -f ".venv" ]]; then
    ENV=$(tail -n 1 .venv)
    echo "Env Conda : ${ENV}"
else
    echo "Base Conda"
    CONDA_DEFAULT_ENV="default"
fi

case "$OSTYPE" in
    darwin*)
        if [[ -z "${ANACONDA_HOME}" ]]; then
        ANACONDA_HOME="/usr/local/Caskroom/mambaforge/base"
        ANACONDA_ENVS="$ANACONDA_HOME/envs"
        fi
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
        ;;
    linux*)
        export MAMBA_EXE="${HOME}/.local/bin/micromamba";
        export MAMBA_ROOT_PREFIX="${JG_CACHE}/mamba";
        __mamba_setup="$($MAMBA_EXE shell hook --shell bash --root-prefix $MAMBA_ROOT_PREFIX 2> /dev/null)"
        if [ $? -eq 0 ]; then
            echo "Running setup"
            eval "$__mamba_setup"
        else
            echo "mamba setup is empty"
            alias micromamba="$MAMBA_EXE"  # Fallback on help from mamba activate
        fi
        # unset __mamba_setup
        ;;
esac

alias mamba="micromamba"
mamba activate "${CONDA_DEFAULT_ENV}"

