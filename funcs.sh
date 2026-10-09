#useful functions

#what this machine is, as the Makefile names it, for where its host
#programs are, obj/<cpu>/<abi>/<os>
OS=`uname`
CPU=`uname -m`
case $CPU in
	x86_64) ABI=AMD64 ;;
	riscv64) ABI=RISCV64 ;;
	loongarch64) CPU=la64; ABI=LA64 ;;
	*) CPU=arm64; ABI=ARM64 ;;
esac

#save terminal state and ensure it is restored on exit or crash
if [ -t 0 ]
then
	saved_stty=$(stty -g 2>/dev/null)
fi

function restore_tty
{
	if [ -t 0 ]
	then
		if [ -n "$saved_stty" ]
		then
			stty "$saved_stty" 2>/dev/null || stty sane 2>/dev/null
		else
			stty sane 2>/dev/null
		fi
	fi
}

trap restore_tty EXIT INT TERM HUP

#a session is the nodes one launch script started, and those nodes
#started in turn. Each has link names of its own, so two sessions on
#one machine do not meet, and a session stops only itself. It lives
#while it has a front, a terminal or a desktop, a way in to it. A
#desktop can be closed and another opened, nodes -g, and when the last
#front has gone the rest of the nodes are stopped.

session_pids=""
session_fronts=""

function session_scan
{
	#every node of this session, and its fronts. A node that starts
	#more nodes, (node-spawn), leaves their pids and link names in a file.
	local todo="$session_pids"
	local pid
	local what
	local val
	scan_pids=""
	scan_fronts="$session_fronts"
	scan_links=""
	scan_files=""
	while [ -n "$todo" ]
	do
		pid=${todo%% *}
		todo=${todo#* }
		if [ -n "$pid" ] && [[ " $scan_pids " != *" $pid "* ]]
		then
			scan_pids+="$pid "
			if [ -f "$TEMP/chrysalisp_$pid.session" ]
			then
				scan_files+="$TEMP/chrysalisp_$pid.session "
				while read what val
				do
					if [ "$what" == "pid" ]
					then
						todo+="$val "
					elif [ "$what" == "front" ]
					then
						todo+="$val "
						scan_fronts+="$val "
					elif [ "$what" == "link" ]
					then
						scan_links+="$val "
					fi
				done < "$TEMP/chrysalisp_$pid.session"
			fi
		fi
	done
}

function session_has_front
{
	#a front is a way in to the session, a terminal or a desktop. A
	#session lives while it has one.
	local pid
	session_scan
	for pid in $scan_fronts
	do
		if kill -0 $pid 2>/dev/null
		then
			return 0
		fi
	done
	return 1
}

function session_stop
{
	#stop every node of this session, and clear away its files
	local pid
	local val
	session_scan
	for pid in $scan_pids
	do
		kill -KILL $pid 2>/dev/null
	done
	for val in $scan_links
	do
		rm -f "$TEMP/$val"
	done
	rm -f $scan_files
}

function session_watch
{
	#stop the session when its last front has gone
	(
		while session_has_front
		do
			sleep 1
		done
		session_stop
	) &> /dev/null &
}

#the launch script starts one node, the first. That node starts the
#rest, in the shape asked for, -t, and as many as asked for, -n, or sized
#to the machine, (node-net) in sys/lisp.inc has the shapes. It leaves
#their pids and link names for this script, and then runs the script it
#was given. The other desktops asked for, -g, $2 of them, are for it to
#start as well, they are the first of the nodes it starts.
function first_run
{
	#input that is not from a keyboard, a pipe or a file, is a script's and
	#not a person's. The terminal is told, and keeps it out of the history
	local piped=""
	if [ ! -t 0 ]
	then
		piped="(defq *tui_scripted* :t) "
	fi
	if [ "${2:-0}" -gt 0 ]
	then
		echo "(progn $piped(node-net :$shape $num_cpu $2 :gui {service/gui/app.lisp}) (import {$1}))"
	else
		echo "(progn $piped(node-net :$shape $num_cpu) (import {$1}))"
	fi
}

#the first node, node 0, is a front. If the launch is in the foreground it is waited for. A watch is then
#left to stop the session when its last front has gone, which is at
#once if this was the only one.
function boot_first
{
	if [ "$front" == "" ] && [ "$1" != "wait" ]
	then
		shift
		"$@" &
		session_pids+="$! "
		session_fronts+="$! "
		session_watch
	else
		shift
		"$@" <&0 &
		local pid=$!
		session_pids+="$pid "
		session_fronts+="$pid "
		wait $pid
		status=$?
		restore_tty
		if session_has_front
		then
			session_watch
		else
			session_stop
		fi
		return $status
	fi
}

function boot_gui
{
	local host="./obj/$CPU/$ABI/$OS/main_gui obj/$CPU/$ABI/sys/boot_image $emu -run"
	if [ $num_gui -eq 0 ]
	then
		#no desktop, a script on the GUI host program
		boot_first nowait $host "$(first_run $script)"
	elif [ "$front" == "" ]
	then
		boot_first nowait $host "$(first_run service/gui/app.lisp $(($num_gui - 1)))"
	else
		boot_first wait $host "$(first_run apps/tui/tui_gui.lisp $(($num_gui - 1)))"
	fi
}

function boot_tui
{
	#a TUI is always waited for, it has the terminal
	boot_first wait ./obj/$CPU/$ABI/$OS/main_tui obj/$CPU/$ABI/sys/boot_image $emu -run "$(first_run $script)"
}

function main
{
	#0 nodes is the network sized to the machine
	num_cpu=0
	num_gui=1
	shape="full"
	emu=""
	front=""
	help=""
	script="apps/tui/tui.lisp"
	while [ "$#" -gt 0 ]; do
	case $1 in
		-i)
			script="apps/tui/install.lisp";
			shift
			;;
		-s)
			script=$2;
			shift 2
			;;
		-e)
			emu=$1;
			shift
			;;
		-f)
			front=$1;
			shift
			;;
		-g)
			num_gui=$2
			shift 2
			;;
		-n)
			num_cpu=$2
			shift 2
			;;
		-t)
			shape=$2
			shift 2
			;;
		*)	echo "[-n cnt] number of nodes, the width of a mesh or a cube, 0 to size to the machine, the default"
			echo "[-t shape] full, ring, star, tree, mesh or cube, full is the default"
			echo "[-g cnt] number of guis"
			echo "[-s script_name] script mode"
			echo "[-e] emulator mode"
			echo "[-f] foreground mode"
			echo "[-h] help"
			help=1
			break
			;;
	esac
	done

	#where the links, and the files of a session, are kept
	export TEMP=/tmp/
	mkdir -p $TEMP
}
