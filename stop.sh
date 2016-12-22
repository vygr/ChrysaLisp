#!/bin/bash
killall main -KILL > /dev/null 2>&1

export TEMP="/tmp/"
mkdir -p $TEMP

# report how many link files are being removed
tfiles=`ls -lU $TEMP/???-??? 2> /dev/null | wc -l`
echo "Cleaning up" $tfiles "link files"

# quietly delete old link files
rm -f $TEMP/???-???
