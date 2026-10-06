#useful functions

OS=`cat os`
CPU=`cat cpu`
ABI=`cat abi`

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
session_links=""
session_salt=$(( ((RANDOM << 15) | RANDOM) * 1021 + RANDOM ))

function link_name
{
	#the name of the link between two nodes, ???-??? in base 36, the
	#form the stop scripts clear away
	local n=$(( ($session_salt + $1 * 128 + $2) % 2176782336 ))
	local digits="0123456789abcdefghijklmnopqrstuvwxyz"
	local name=""
	for ((i=0; i<6; i++))
	do
		name="${digits:$(($n % 36)):1}$name"
		n=$(($n / 36))
	done
	nl="${name:0:3}-${name:3:3}"
}

function add_link
{
	if [ $1 != $2 ]
	then
		if [ $1 -lt $2 ]
		then
			link_name $1 $2
		else
			link_name $2 $1
		fi
		if [[ "$links" != *"$nl"* ]]
		then
			links+="-l $nl "
		fi
		if [[ "$session_links" != *"$nl"* ]]
		then
			session_links+="$nl "
		fi
	fi
}

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
	scan_links="$session_links"
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

function wrap
{
	wp=$(($1 % $num_cpu))
	if [ $wp -lt 0 ]
	then
		wp=$(($wp + $num_cpu))
	fi
}

#with -n 0 the first node sizes the network to the machine. It starts
#the other nodes, and then runs the script it was given.
function auto_run
{
	if [ "$auto" == "" ]
	then
		echo "$1"
	else
		echo "(progn (node-auto) (import {$1}))"
	fi
}

#the first node, node 0, is a front, and is the last to be booted.
#If the launch is in the foreground it is waited for. A watch is then
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

function boot_cpu_gui
{
	if [ $num_gui -eq 0 ]
	then
		if [ $1 -lt 1 ]
		then
			boot_first nowait ./obj/$CPU/$ABI/$OS/main_gui obj/$CPU/$ABI/sys/boot_image $2 $emu -run "$(auto_run $script)"
			return $?
		else
			./obj/$CPU/$ABI/$OS/main_gui obj/$CPU/$ABI/sys/boot_image $2 $emu &
			session_pids+="$! "
			disown $!
		fi
	elif [ $1 -lt $num_gui ]
	then
		if [ $1 -ge 1 ]
		then
			./obj/$CPU/$ABI/$OS/main_gui obj/$CPU/$ABI/sys/boot_image $2 $emu -run "$(auto_run service/gui/app.lisp)" &
			session_pids+="$! "
			session_fronts+="$! "
			disown $!
		elif [ "$front" == "" ]
		then
			boot_first nowait ./obj/$CPU/$ABI/$OS/main_gui obj/$CPU/$ABI/sys/boot_image $2 $emu -run "$(auto_run service/gui/app.lisp)"
			return $?
		else
			boot_first wait ./obj/$CPU/$ABI/$OS/main_gui obj/$CPU/$ABI/sys/boot_image $2 $emu -run "$(auto_run apps/tui/tui_gui.lisp)"
			return $?
		fi
	else
		./obj/$CPU/$ABI/$OS/main_gui obj/$CPU/$ABI/sys/boot_image $2 $emu &
		session_pids+="$! "
		disown $!
	fi
}

function boot_cpu_tui
{
	if [ $1 -lt 1 ]
	then
		#a TUI is always waited for, it has the terminal
		boot_first wait ./obj/$CPU/$ABI/$OS/main_tui obj/$CPU/$ABI/sys/boot_image $2 $emu -run "$(auto_run $script)"
		return $?
	else
		./obj/$CPU/$ABI/$OS/main_tui obj/$CPU/$ABI/sys/boot_image $2 $emu &
		session_pids+="$! "
		disown $!
	fi
}

function main
{
	num_cpu=$1
	max_cpu=$2
	shift 2
	num_gui=1
	emu=""
	front=""
	auto=""
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
			if [ $num_cpu -eq 0 ]
			then
				auto=1
				num_cpu=1
			fi
			shift 2
			;;
		*)	echo "[-n cnt] number of nodes, 0 to size to the machine"
			echo "[-g cnt] number of guis"
			echo "[-s script_name] script mode"
			echo "[-e] emulator mode"
			echo "[-f] foreground mode"
			echo "[-h] help"
			num_cpu=0
			break
			;;
	esac
	done

	#not greater than $max_cpu
	if [ $num_cpu -gt $max_cpu ]
	then
		num_cpu=$max_cpu
	fi

	#where the links, and the files of a session, are kept
	export TEMP=/tmp/
	mkdir -p $TEMP
}
