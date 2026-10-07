#!/bin/bash

# stop every node on this machine, whoever started it. A session
# stops itself when its last terminal or desktop goes, see funcs.sh,
# this is for when one has not.
killall main_gui -KILL &> /dev/null
killall main_tui -KILL &> /dev/null

export TEMP=/tmp/
mkdir -p $TEMP

# quietly delete old link files, and the files of old sessions
rm -f $TEMP/???-???
rm -f $TEMP/chrysalisp_*.session

# shared memory a node made, for pixels, is not a file to delete. The
# host program lets go of what dead nodes left
for host in obj/*/*/*/main_tui obj/*/*/*/main_gui
do
	if [ -x "$host" ]
	then
		"$host" -shm_sweep 2>/dev/null
		break
	fi
done

if [ -t 0 ]
then
	stty sane 2>/dev/null
fi
