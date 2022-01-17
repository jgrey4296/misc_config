#!/usr/bin/env bash
# Ensure the shell can use conda:
source ~/anaconda3/etc/profile.d/conda.sh

conda update conda -y

conda update --all -y
conda env export > ~/.shell_files/conda_envs/base_env.yaml

for f in ~/.shell_files/conda_envs/*.yaml; do
    name=`basename -s .yaml $f`
    echo "Found $name"
    if conda activate $name; then
        echo "Loaded $name"
        conda update --all -y
        conda env export > $f
        conda deactivate
    fi
done
