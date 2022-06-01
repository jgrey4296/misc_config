#!/usr/bin/env bash
# Get the User CPU Usage
# and warn if it is above MAX %
#
#
#
#
set -eo pipefail

if [ $# > 1 ]
then
    CPU_MAX="$1"
fi

if [ -z $CPU_MAX ]
then
    CPU_MAX="50"

fi

echo "CPU Max: $CPU_MAX"
CPU=( $(top -l1 -n0 | awk '/CPU/ {print $0}') )
UserPerc=( ${CPU[2]} )
TESTSTR="${UserPerc[0]/\%/} > ${CPU_MAX}"
HIGH=$(echo $TESTSTR | bc)

echo "$TESTSTR : $HIGH"
if [ $HIGH -eq 1 ]
then
    say -v Moira -r 50 "CPU Usage is higher than ${CPU_MAX} %"
else
    echo "CPU Usage is fine"
fi
