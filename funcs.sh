#useful functions

function zero_pad
{
	if [ $1 -lt 10 ]
	then
		zp="0$1"
	else
		zp=$1
	fi
}

function add_link
{
	if [ $1 != $2 ]
	then
		if [ $1 -lt $2 ]
		then
			links+="-l /$1-$2 "
		else
			links+="-l /$2-$1 "
		fi
	fi
}

function wrap
{
	wp=$(($1 % $num_cpu))
	if [ $1 -lt 0 ]
	then
		wp=$(($wp + $num_cpu))
	fi
}

function boot_cpu
{
	if [ $1 == 0 ]
	then
		./main -cpu $1 $2 -run tests/test1 &
	else
		./main -cpu $1 $2 &
	fi
}
