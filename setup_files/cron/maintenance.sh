#!/usr/bin/env bash
set -euo pipefail

# General maintenance scripts
# Update node, brew, doom etc
echo "Conda --------------------"
~/.doom.d/setup_files/cron/conda_maintenance.sh

echo "Brew Cleanup --------------------"
brew cleanup
echo "--------------------"

echo "Brew Update --------------------"
brew update
echo "--------------------"

echo "Doom Upgrade --------------------"
doom upgrade
echo "--------------------"

echo "CABAL Update--------------------"
cabal update
echo "--------------------"

echo "NPM Update--------------------"
npm update npm -g
echo "--------------------"
