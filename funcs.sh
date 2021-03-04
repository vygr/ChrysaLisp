#useful functions

OS=`cat platform`
CPU=`cat arch`
ABI=`cat abi`

function zero_pad
{
	if [ $1 -lt 100 ]
	then
		zp="0$1"
	else
		zp=$1
	fi
	if [ $zp -lt 10 ]
	then
		zp="0$zp"
	fi
}

function add_link
{
	zero_pad $(($1 + $base_cpu))
	l1=$zp
	zero_pad $(($2 + $base_cpu))
	l2=$zp
	if [ $l1 != $l2 ]
	then
		if [ $l1 -lt $l2 ]
		then
			nl=$l1-$l2
		else
			nl=$l2-$l1
		fi
		if [[ "$links" != *"$nl"* ]]
		then
			links+="-l $nl "
		fi
	fi
}

function wrap
{
	wp=$(($1 % $num_cpu))
	if [ $wp -lt 0 ]
	then
		wp=$(($wp + $num_cpu))
	fi
}

function boot_cpu_gui
{
	if [ $1 -lt 1 ]
	then
		./obj/$CPU/$ABI/$OS/main_gui obj/$CPU/$ABI/sys/boot_image $2 $3 -run gui/gui/gui.lisp &
	else
		./obj/$CPU/$ABI/$OS/main_gui obj/$CPU/$ABI/sys/boot_image $2 $3 &
	fi
}

function boot_cpu_tui
{
	if [ $1 -lt 1 ]
	then
		./obj/$CPU/$ABI/$OS/main_tui obj/$CPU/$ABI/sys/boot_image $2 $3 -run apps/terminal/tui.lisp
	else
		./obj/$CPU/$ABI/$OS/main_tui obj/$CPU/$ABI/sys/boot_image $2 $3 &
	fi
}

./stop.sh
