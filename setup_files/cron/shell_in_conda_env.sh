#!/usr/bin/env bash
# REMINDER: USE bash -i {script}
# to run scripts like this
set -euo pipefail

conda activate bookmark
python ~/.doom.d/setup_files/cron/auto_bibtex_tweet.py
