#!/bin/bash
# Run this with bash to set up a new machine:

echo "Setting Up System"

SHELL_DIR="$(dirname $0 | xargs realpath)"

echo "Dir of Script: $SHELL_DIR"
echo "----------"
echo "Setting up main Symlinks:"
ln -s "$SHELL_DIR" ~/.shell_files
ln -s "$(dirname $SHELL_DIR)" ~/.spacemacs.d

echo ""
ln -s "$SHELL_DIR/.condarc" ~/.condarc
ln -s "$SHELL_DIR/.npmrc" ~/.npmrc
ln -s "$SHELL_DIR/.gitconfig" ~/.gitconfig

echo ""
echo "Appending bash setup to bashrc"
echo "source ~/.shell_files/main_bash" >> ~/.bashrc

echo "Setting up Conda Environments"
for Yaml in "$(find $SHELL_DIR/conda_envs/*.yaml)"
do
    echo "Found Yaml: $Yaml"
    conda env create -f $Yaml
done


echo "Completed Setup"
echo "Don't forget to create ~/mega and ~/github"
