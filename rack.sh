#!/bin/bash

# rack.sh up|down|status|run command line
# this machine's member of the mesh, a node that stays up, finds the other
# machines, links to them and takes a sync, lib/rack/member.lisp.
#   up      start it, if it is not up
#   down    stop it, and the nodes it started, and no others. stop.sh
#           stops every node of the machine
#   status  is it up
#   run     have it run a command line, and say what came of it, as
#           ./rack.sh run 'rack "tests -a"'
# It joins the mesh and takes a sync, so a machine it is up on can be
# written to, within its tree, by any machine that has its key, see
# docs/ai_digest/rack.md. It is not started unless asked for.
#
# It is kept up by a loop of the shell. A member that has taken a newer
# waypoint by the herd, on a tree with no .git, leaves the file
# chrysalisp_rack_update and ends. The loop then stops the nodes it
# started, and no others, does make install, and starts it again on what
# was built. If the install fails what was there before is put back. A
# member that ends with no such file, down or a fault, is not started again.

export TEMP=/tmp/
pid=$(cat $TEMP/chrysalisp_rack.pid 2>/dev/null)
loop=$(cat $TEMP/chrysalisp_rack_loop.pid 2>/dev/null)
up=""
if [ -n "$pid" ] && kill -0 $pid 2>/dev/null
then
	up=1
fi
if [ -n "$loop" ] && kill -0 $loop 2>/dev/null
then
	up=1
else
	loop=""
fi

#stop a member, and the nodes it was asked to start, nodes -a, or a
#desktop, nodes -g, with their links. It left a note of them, as every
#node that starts nodes does, see funcs.sh
function member_stop
{
	local todo="$1 "
	local seen=""
	while [ -n "$todo" ]
	do
		local one=${todo%% *}
		todo=${todo#* }
		if [ -n "$one" ] && [[ " $seen " != *" $one "* ]]
		then
			seen+="$one "
			if [ -f "$TEMP/chrysalisp_$one.session" ]
			then
				while read what val
				do
					if [ "$what" == "link" ]
					then
						rm -f "$TEMP/$val"
					else
						todo+="$val "
					fi
				done < "$TEMP/chrysalisp_$one.session"
				rm -f "$TEMP/chrysalisp_$one.session"
			fi
			kill -KILL $one 2>/dev/null
		fi
	done
}

#the member, started again each time it asks to be after a make install
function member_loop
{
	while :
	do
		rm -f $TEMP/chrysalisp_rack_update
		local host=$(ls obj/*/*/*/main_tui 2>/dev/null | head -1)
		[ -z "$host" ] && break
		./$host $(dirname $(dirname $host))/sys/boot_image -run lib/rack/member.lisp \
			>> $TEMP/chrysalisp_rack.log 2>&1 < /dev/null &
		local member=$!
		echo $member > $TEMP/chrysalisp_rack.pid
		wait $member
		[ -f $TEMP/chrysalisp_rack_update ] || break
		member_stop $member
		#what runs now is kept, to be put back if the install fails
		local os_dir=$(dirname $host) sys_dir=$(dirname $(dirname $host))/sys
		rm -rf $TEMP/chrysalisp_rack_keep
		mkdir -p $TEMP/chrysalisp_rack_keep
		cp -Rp $os_dir $TEMP/chrysalisp_rack_keep/os
		cp -p $sys_dir/boot_image $TEMP/chrysalisp_rack_keep/boot_image
		echo "update: make install, $(date)" >> $TEMP/chrysalisp_rack.log
		if ! make install >> $TEMP/chrysalisp_rack.log 2>&1 || [ -z "$(ls obj/*/*/*/main_tui 2>/dev/null)" ]
		then
			echo "update: make install failed, what was there is put back" >> $TEMP/chrysalisp_rack.log
			mkdir -p $os_dir $sys_dir
			cp -Rp $TEMP/chrysalisp_rack_keep/os/. $os_dir/
			cp -p $TEMP/chrysalisp_rack_keep/boot_image $sys_dir/boot_image
		fi
		rm -rf $TEMP/chrysalisp_rack_keep
	done
	rm -f $TEMP/chrysalisp_rack.pid $TEMP/chrysalisp_rack_loop.pid
}

what=$1
shift
case $what in
	up)
		if [ -n "$up" ]
		then
			echo "up already, pid $pid"
		else
			host=$(ls obj/*/*/*/main_tui 2>/dev/null | head -1)
			if [ -z "$host" ]
			then
				echo "no host program, make install"
				exit 1
			fi
			: > $TEMP/chrysalisp_rack.log
			rm -f $TEMP/chrysalisp_rack.pid
			nohup ./rack.sh loop < /dev/null > /dev/null 2>&1 &
			echo $! > $TEMP/chrysalisp_rack_loop.pid
			#the member says its own pid as it starts
			n=0
			while [ ! -f $TEMP/chrysalisp_rack.pid ] && [ $n -lt 50 ]
			do
				sleep 0.1
				n=$((n+1))
			done
			echo "started, pid $(cat $TEMP/chrysalisp_rack.pid 2>/dev/null)"
		fi
		;;
	down)
		if [ -n "$up" ]
		then
			#the loop first, so it does not start it again
			[ -n "$loop" ] && kill -KILL $loop 2>/dev/null
			[ -n "$pid" ] && member_stop $pid
			rm -f $TEMP/chrysalisp_rack.pid $TEMP/chrysalisp_rack_loop.pid $TEMP/chrysalisp_rack_update
			echo "stopped, pid $pid"
		else
			echo "was not up"
		fi
		;;
	status)
		if [ -n "$pid" ] && kill -0 $pid 2>/dev/null
		then
			echo "up, pid $pid"
		elif [ -n "$up" ]
		then
			echo "updating, make install, it starts again after"
		else
			echo "down"
		fi
		;;
	run)
		if [ -z "$up" ]
		then
			echo "not up, ./rack.sh up"
			exit 1
		fi
		rm -f $TEMP/chrysalisp_rack_result
		echo "$*" > $TEMP/chrysalisp_rack_job.tmp
		mv -f $TEMP/chrysalisp_rack_job.tmp $TEMP/chrysalisp_rack_job
		while kill -0 $pid 2>/dev/null
		do
			if [ -f $TEMP/chrysalisp_rack_result ] && grep -q "^DONE" $TEMP/chrysalisp_rack_result
			then
				grep -v "^DONE$" $TEMP/chrysalisp_rack_result
				exit 0
			fi
			sleep 0.1
		done
		echo "the member has gone"
		exit 1
		;;
	loop)
		#what up starts, by nohup, so it outlives the shell it was asked from
		member_loop
		;;
	*)
		echo "rack.sh up|down|status|run command line"
		exit 1
		;;
esac
