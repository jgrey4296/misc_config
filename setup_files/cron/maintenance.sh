#!/usr/bin/env bash
set -euo pipefail

# General maintenance scripts
# Update node, brew, doom etc
brew update
npm update npm -g
doom upgrade
