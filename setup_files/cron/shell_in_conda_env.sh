#!/usr/bin/env bash
# REMINDER: USE bash -i {script}
# to run scripts like this
set -euo pipefail

conda init
conda activate bookmark

python ./auto_bibtex_tweet.py
