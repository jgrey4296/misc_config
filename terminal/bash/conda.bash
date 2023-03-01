#https://stackoverflow.com/questions/36365801/run-a-crontab-job-using-an-anaconda-env/60977676#60977676
# This is how to activate conda if it
# complains the shell isn't set up:
# source $HOME/anaconda/etc/profile.d/conda.sh

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/opt/anaconda3/envs/base310/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/opt/anaconda3/envs/base310/etc/profile.d/conda.sh" ]; then
        . "/opt/anaconda3/envs/base310/etc/profile.d/conda.sh"
    else
        export PATH="/opt/anaconda3/envs/base310/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<
CONDA_DEFAULT_ENV=bookmark
