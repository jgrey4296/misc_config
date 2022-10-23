#!/bin/bash
# Run this with bash to set up a new machine:

echo "Setting Up System"

SHELL_DIR="$(dirname $0 | xargs realpath)"

echo "Dir of Script: $SHELL_DIR"
echo "----------"
echo "Setting up main Symlinks:"
ln -s "$SHELL_DIR" ~/.shell_files
ln -s "$(dirname $SHELL_DIR)" ~/.doom.d

echo "Appending bash setup to bashrc"
echo "source ~/.bash_profile" >> ~/.bashrc

echo "Appending bash setup to bash_profile"
echo "source ~/.setup_files/main.bash" >> ~/.bash_profile


echo "Setting up tool config symlinks"
for fname in $(find ~/.shell_files/tool_configs -type f -name ".*")
do
    basefile=$(basename "$fname")
    ln -s $fname ~/$basefile
done
ln -s ~/.shell_files/tool_configs/gradle.properties ~/gradle.properties

echo "Linking snippets"
ln -s "$SHELL_DIR/../snippets" ~/.emacs.d/private/snippets

echo "Linking anaconda"
ln -s /opt/anaconda3 ~/.anaconda
ln -s /opt/anaconda3 ~/.anaconda3

echo "Setting up Conda Environments"
for Yaml in $(find $SHELL_DIR/conda_envs/ -type f -name "*.yaml")
do
    echo "Found Yaml: $Yaml"
    conda env create -f $Yaml
done

echo "Setting up 24-bit emacs"
tic -x -o ~/.terminfo ~/.shell_files/tool_configs/terminfo-24bit.src

echo "Completed Setup"
echo "Don't forget to create ~/mega and ~/github"
echo "and setup ~/.authinfo"
