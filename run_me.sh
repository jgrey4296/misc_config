#!/bin/bash
# Run this with bash to set up a new machine:

echo "Setting Up System"

SHELL_DIR="$(dirname $0 | xargs realpath)"

echo "Dir of Script: $SHELL_DIR"
echo "----------"

echo "Appending bash setup to bashrc"
echo "source $HOME/.bash_profile" >> "$HOME/.bashrc"

echo "Appending bash setup to bash_profile"
echo "source $HOME/.doom.d/terminal/main.bash" >> "$HOME/.bash_profile"


echo "Setting up tool config symlinks"
for fname in $(find $HOME/.doom.d/terminal/tool_configs -type f -name ".*")
do
    basefile=$(basename "$fname")
    ln -s "$fname" "$HOME/$basefile"
done
ln -s "$HOME/.doom.d/terminal/tool_configs/gradle.properties" "$HOME/gradle.properties"

echo "Linking snippets"
ln -s "$HOME/.doom.d/templates/code" "$HOME/.emacs.d/private/snippets"

echo "Linking anaconda"
ln -s "/opt/anaconda3" "$HOME/.anaconda"

echo "Setting up Conda Environments"
for Yaml in $(find $HOME/.doom.d/terminal/conda_envs/ -type f -name "*.yaml")
do
    echo "Found Yaml: $Yaml"
    conda env create -f "$Yaml"
done

echo "Setting up 24-bit emacs"
tic -x -o "$HOME/.terminfo" "$HOME/.doom.d/terminal/tool_configs/terminfo-24bit.src"

echo "Completed Setup"
echo "Don't forget to create $HOME/mega and $HOME/github"
echo "and setup $HOME/.authinfo"
