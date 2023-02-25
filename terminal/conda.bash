#https://stackoverflow.com/questions/36365801/run-a-crontab-job-using-an-anaconda-env/60977676#60977676

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

# from: https://itecnotes.com/server/cron-how-to-use-the-aliases-in-the-crontab/
shopt -s expand_aliases
source "$HOME/.doom.d/terminal/bash_setup/_basic_utils.bash"
source "$HOME/.doom.d/terminal/bash_setup/_base_path.bash"
source "$HOME/.doom.d/terminal/bash_setup/cron.bash"
source "$HOME/.doom.d/terminal/bash_setup/latex.bash"
source "$HOME/.doom.d/terminal/bash_setup/python.bash"
source "$HOME/.doom.d/terminal/bash_setup/ruby.bash"
source "$HOME/.doom.d/terminal/bash_setup/_exports.bash"

CONDA_DEFAULT_ENV=bookmark
conda activate "$CONDA_DEFAULT_ENV"
