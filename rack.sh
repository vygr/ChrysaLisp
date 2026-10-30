#!/bin/bash

# rack.sh up [-f]|down|status|run command line
# this machine's member of the mesh, a node that stays up, finds the other
# machines, links to them and takes a sync, lib/rack/member.lisp.
#   up      start it, if it is not up
#   up -f   start it to follow the tree's git branch, a clone kept for it
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
#
# Followed, up -f, a tree with a .git updates itself from where it was
# cloned. Each 10 minutes the loop fetches its branch. If that has moved
# on, the tree is clean, nothing of its own, and the new commit follows
# from the one it is at, the member is stopped, the tree fast forwarded,
# and make install done. The member then notes the waypoint, which the
# herd takes on to the machines with no .git, and starts its store if the
# machine is a member of one. If the install fails the tree is put back
# to the commit it was at and built again, and that commit is not tried
# again, a newer one is. Every machine updates by itself from git, so a
# change to what goes between machines, that parts old from new, reaches
# them all. What it did is in the log, /tmp/chrysalisp_rack.log.

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

#followed, the commit the tree's branch is at, if the tree is to be taken
#to it: newer, it follows from the tree's, the tree is clean, and it was
#not tried and failed. Nothing if not
function follow_new
{
	git fetch -q 2>/dev/null || return
	local up=$(git rev-parse -q --verify '@{u}' 2>/dev/null)
	local at=$(git rev-parse -q --verify HEAD 2>/dev/null)
	[ -z "$up" ] || [ -z "$at" ] || [ "$up" == "$at" ] && return
	[ "$up" == "$(cat $TEMP/chrysalisp_rack_skip 2>/dev/null)" ] && return
	git merge-base --is-ancestor $at $up 2>/dev/null || return
	if [ -n "$(git status --porcelain 2>/dev/null)" ]
	then
		echo "follow: $up not taken, the tree has changes of its own, $(date)" >> $TEMP/chrysalisp_rack.log
		return
	fi
	echo $up
}

#wait for the member to end, and followed, look each 10 minutes for a
#newer commit, and if there is one stop it and say so
function member_wait
{
	local member=$1
	#the first look soon after it starts, so a machine that was off catches up
	local next=$(( $(date +%s) + ${CHRYSALISP_FOLLOW_FIRST:-60} ))
	while kill -0 $member 2>/dev/null
	do
		if [ -f $TEMP/chrysalisp_rack_follow ] && [ $(date +%s) -ge $next ]
		then
			next=$(( $(date +%s) + ${CHRYSALISP_FOLLOW_EVERY:-600} ))
			local new=$(follow_new)
			if [ -n "$new" ]
			then
				echo "follow $new" > $TEMP/chrysalisp_rack_update
				member_stop $member
				break
			fi
		fi
		sleep 5
	done
	wait $member 2>/dev/null
}

#the member, started again each time it asks to be after a make install
function member_loop
{
	while :
	do
		rm -f $TEMP/chrysalisp_rack_update
		local host=$(ls obj/*/*/*/chrysalisp 2>/dev/null | head -1)
		[ -z "$host" ] && break
		./$host $(dirname $(dirname $host))/sys/boot_image -run lib/rack/member.lisp \
			>> $TEMP/chrysalisp_rack.log 2>&1 < /dev/null &
		local member=$!
		echo $member > $TEMP/chrysalisp_rack.pid
		member_wait $member
		[ -f $TEMP/chrysalisp_rack_update ] || break
		member_stop $member
		#followed, the tree is taken to the new commit first
		local was="" new=""
		if [[ "$(cat $TEMP/chrysalisp_rack_update)" == follow* ]]
		then
			new=$(cut -d' ' -f2 $TEMP/chrysalisp_rack_update)
			was=$(git rev-parse HEAD)
			if ! git merge -q --ff-only $new >> $TEMP/chrysalisp_rack.log 2>&1
			then
				echo "follow: $new could not be taken, $(date)" >> $TEMP/chrysalisp_rack.log
				echo $new > $TEMP/chrysalisp_rack_skip
				continue
			fi
			echo "follow: ${was:0:9} to ${new:0:9}, $(date)" >> $TEMP/chrysalisp_rack.log
		fi
		#what runs now is kept, to be put back if the install fails
		local os_dir=$(dirname $host) sys_dir=$(dirname $(dirname $host))/sys
		rm -rf $TEMP/chrysalisp_rack_keep
		mkdir -p $TEMP/chrysalisp_rack_keep
		cp -Rp $os_dir $TEMP/chrysalisp_rack_keep/os
		cp -p $sys_dir/boot_image $TEMP/chrysalisp_rack_keep/boot_image
		echo "update: make install, $(date)" >> $TEMP/chrysalisp_rack.log
		if ! make install >> $TEMP/chrysalisp_rack.log 2>&1 || [ -f obj/install_failed ] \
			|| [ -z "$(ls obj/*/*/*/chrysalisp 2>/dev/null)" ]
		then
			if [ -n "$was" ]
			then
				#followed, back to the commit it was at, built again, and the
				#one that failed is not tried again
				echo "follow: ${new:0:9} failed to build, back to ${was:0:9}" >> $TEMP/chrysalisp_rack.log
				echo $new > $TEMP/chrysalisp_rack_skip
				git reset -q --hard $was >> $TEMP/chrysalisp_rack.log 2>&1
			fi
			if [ -z "$was" ] || ! make install >> $TEMP/chrysalisp_rack.log 2>&1 \
				|| [ -f obj/install_failed ] || [ -z "$(ls obj/*/*/*/chrysalisp 2>/dev/null)" ]
			then
				echo "update: make install failed, what was there is put back" >> $TEMP/chrysalisp_rack.log
				mkdir -p $os_dir $sys_dir
				cp -Rp $TEMP/chrysalisp_rack_keep/os/. $os_dir/
				cp -p $TEMP/chrysalisp_rack_keep/boot_image $sys_dir/boot_image
			fi
		fi
		rm -rf $TEMP/chrysalisp_rack_keep
		#followed, git may have changed this file: the loop goes on as the
		#new one, the same process, so its pid stays as it was
		[ -n "$new" ] && exec ./rack.sh loop
	done
	rm -f $TEMP/chrysalisp_rack.pid $TEMP/chrysalisp_rack_loop.pid
}

what=$1
shift
case $what in
	up)
		if [ "$1" == "-f" ] && [ -z "$(git rev-parse -q --verify '@{u}' 2>/dev/null)" ]
		then
			echo "-f follows a git clone's branch, and this tree has none"
			exit 1
		fi
		if [ -n "$up" ]
		then
			echo "up already, pid $pid"
		else
			if [ "$1" == "-f" ]
			then
				: > $TEMP/chrysalisp_rack_follow
			else
				rm -f $TEMP/chrysalisp_rack_follow
			fi
			host=$(ls obj/*/*/*/chrysalisp 2>/dev/null | head -1)
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
			rm -f $TEMP/chrysalisp_rack.pid $TEMP/chrysalisp_rack_loop.pid $TEMP/chrysalisp_rack_update \
				$TEMP/chrysalisp_rack_follow
			echo "stopped, pid $pid"
		else
			echo "was not up"
		fi
		;;
	status)
		if [ -n "$pid" ] && kill -0 $pid 2>/dev/null
		then
			if [ -f $TEMP/chrysalisp_rack_follow ]
			then
				echo "up, pid $pid, following $(git rev-parse --abbrev-ref '@{u}' 2>/dev/null) at $(git rev-parse --short HEAD)"
			else
				echo "up, pid $pid"
			fi
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
		#the member writes what the job says as it is said: it is shown as
		#it comes, whole lines, till the DONE it ends with
		shown=0
		while kill -0 $pid 2>/dev/null
		do
			if [ -f $TEMP/chrysalisp_rack_result ]
			then
				done=$(grep -c "^DONE$" $TEMP/chrysalisp_rack_result)
				lines=$(wc -l < $TEMP/chrysalisp_rack_result)
				if [ $lines -gt $shown ]
				then
					sed -n "$((shown + 1)),${lines}p" $TEMP/chrysalisp_rack_result | grep -v "^DONE$"
					shown=$lines
				fi
				[ $done -gt 0 ] && exit 0
			fi
			sleep 0.1
		done
		echo "the member has gone"
		exit 1
		;;
	loop)
		#what up starts, by nohup, so it outlives the shell it was asked from.
		#bash reads a file as it goes, and git may have changed this one, so
		#nothing after the loop is read from it
		member_loop
		exit 0
		;;
	*)
		echo "rack.sh up [-f]|down|status|run command line"
		exit 1
		;;
esac
