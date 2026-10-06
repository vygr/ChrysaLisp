#!/bin/bash

# stop every node on this machine, whoever started it. A session
# stops itself when its first node goes, see funcs.sh, this is for
# when one has not.
killall main_gui -KILL &> /dev/null
killall main_tui -KILL &> /dev/null

export TEMP=/tmp/
mkdir -p $TEMP

# quietly delete old link files, and the files of old sessions
rm -f $TEMP/???-???
rm -f $TEMP/chrysalisp_*.session

if [ -t 0 ]
then
	stty sane 2>/dev/null
fi
