#!/bin/bash

#have we got a paramater ?
if [ -z ${1+x} ]
then
	num_side=3
else
	num_side=$1
fi

#less than 3 cpu across ?
if [ $num_side -lt 3 ]
then
	./run.sh $(($1*$1))
	exit
fi

#not greater then 100
if [ $num_side -gt 10 ]
then
	num_side=10
fi

num_cpu=$(($num_side*$num_side))

for ((cpu_y=$num_side-1; cpu_y>=0; cpu_y--))
do
	for ((cpu_x=$num_side-1; cpu_x>=0; cpu_x--))
	do
		cpu=$(($cpu_y*$num_side+$cpu_x))
		links=""
		for ((lcpu_y=$cpu_y-1; lcpu_y<=$cpu_y+1; lcpu_y++))
		do
			c2y=$lcpu_y
			if [ $c2y == -1 ]
			then
				c2y=$(($num_side-1))
			fi
			if [ $c2y == $num_side ]
			then
				c2y=0
			fi

			c1=$cpu
			c2=$(($c2y*$num_side+$cpu_x))

			if [ $c1 -lt 10 ]
			then
				c1="0$c1"
			fi
			if [ $c2 -lt 10 ]
			then
				c2="0$c2"
			fi
			if [ $c1 != $c2 ]
			then
				if [ $c1 -lt $c2 ]
				then
					links+="-l /$c1-$c2 "
				else
					links+="-l /$c2-$c1 "
				fi
			fi
		done
		for ((lcpu_x=$cpu_x-1; lcpu_x<=$cpu_x+1; lcpu_x++))
		do
			c2x=$lcpu_x
			if [ $c2x == -1 ]
			then
				c2x=$(($num_side-1))
			fi
			if [ $c2x == $num_side ]
			then
				c2x=0
			fi

			c1=$cpu
			c2=$(($cpu_y*$num_side+$c2x))

			if [ $c1 -lt 10 ]
			then
				c1="0$c1"
			fi
			if [ $c2 -lt 10 ]
			then
				c2="0$c2"
			fi
			if [ $c1 != $c2 ]
			then
				if [ $c1 -lt $c2 ]
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
done
