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

export TEMP=/tmp/
pid=$(cat $TEMP/chrysalisp_rack.pid 2>/dev/null)
up=""
if [ -n "$pid" ] && kill -0 $pid 2>/dev/null
then
	up=1
fi

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
			nohup ./$host $(dirname $(dirname $host))/sys/boot_image -run lib/rack/member.lisp \
				> $TEMP/chrysalisp_rack.log 2>&1 < /dev/null &
			echo $! > $TEMP/chrysalisp_rack.pid
			echo "started, pid $!"
		fi
		;;
	down)
		if [ -n "$up" ]
		then
			#and the nodes it was asked to start, nodes -a, or a desktop,
			#nodes -g, with their links. It left a note of them, as
			#every node that starts nodes does, see funcs.sh
			todo="$pid "
			seen=""
			while [ -n "$todo" ]
			do
				one=${todo%% *}
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
			rm -f $TEMP/chrysalisp_rack.pid
			echo "stopped, pid $pid"
		else
			echo "was not up"
		fi
		;;
	status)
		if [ -n "$up" ]
		then
			echo "up, pid $pid"
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
	*)
		echo "rack.sh up|down|status|run command line"
		exit 1
		;;
esac
