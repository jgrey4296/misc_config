#!/usr/bin/env bash
set -euo pipefail

# General maintenance scripts
# Update node, brew, doom etc
brew update
doom upgrade
npm update npm -g
