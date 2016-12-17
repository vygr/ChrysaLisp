#useful functions

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
	if [ $1 != $2 ]
	then
		if [ $1 -lt $2 ]
		then
			nl=$1-$2
		else
			nl=$2-$1
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
		./obj/main -cpu $1 $2 -run gui/gui &
	else
		./obj/main -cpu $1 $2 &
	fi
	echo -cpu $1 $2
}

function boot_cpu_tui
{
	if [ $1 -lt 1 ]
	then
		./obj/main -cpu $1 $2 -run apps/terminal/tui
	else
		./obj/main -cpu $1 $2 &
	fi
	echo -cpu $1 $2
}

./stop.sh
