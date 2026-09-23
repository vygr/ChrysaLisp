#!/bin/bash
killall main_gui -KILL &> /dev/null
killall main_tui -KILL &> /dev/null

export TEMP=/tmp/
mkdir -p $TEMP

# quietly delete old link files
rm -f $TEMP/???-???

if [ -t 0 ]
then
	stty sane 2>/dev/null
fi
