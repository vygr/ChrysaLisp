#!/bin/bash
killall main -KILL > /dev/null 2>&1

# report how many link files are being removed
tfiles=`ls -lU /tmp/???-??? 2> /dev/null | wc -l`
echo "Cleaning up" $tfiles "link files"

# quietly delete old link files
rm -f /tmp/???-???
