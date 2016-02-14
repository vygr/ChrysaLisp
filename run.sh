#!/bin/bash

#have we got a paramater ?
if [ -z ${1+x} ]
then
	num_cpu=8
else
	num_cpu=$1
fi

#not greater then 100
if [ $num_cpu -gt 100 ]
then
	num_cpu=100
fi

for ((cpu=$num_cpu-1; cpu>=0; cpu--))
do
	links=""
	for ((lcpu=0; lcpu<$num_cpu; lcpu++))
	do
		c1=$cpu
		c2=$lcpu
		if [ $c1 -lt 10 ]
		then
			c1="0$c1"
		fi
		if [ $c2 -lt 10 ]
		then
			c2="0$c2"
		fi
		if [ $lcpu != $cpu ]
		then
			if [ $cpu -lt $lcpu ]
			then
				links+="-l /$c1-$c2 "
			else
				links+="-l /$c2-$c1 "
			fi
		fi
	done
	if [ $cpu == 0 ]
	then
		./main -cpu $cpu $links -run tests/test1 &
	else
		./main -cpu $cpu $links &
	fi
done
