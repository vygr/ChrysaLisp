#!/bin/bash

#have we got a paramater ?
if [ -z ${1+x} ]
then
	num_cpu=8
else
	num_cpu=$1
fi

#less than 3 cpu ?
if [ $num_cpu -lt 3 ]
then
	./run.sh $1
	exit
fi

#not greater then 100
if [ $num_cpu -gt 100 ]
then
	num_cpu=100
fi

source funcs.sh

for ((cpu=$num_cpu-1; cpu>=0; cpu--))
do
	links=""
	for ((lcpu=$cpu-1; lcpu<=$cpu+1; lcpu++))
	do
		c1=$cpu
		wrap $lcpu
		c2=$wp

		zero_pad $c1
		c1=$zp
		zero_pad $c2
		c2=$zp
		add_link $c1 $c2
	done
	boot_cpu $cpu "$links"
done
