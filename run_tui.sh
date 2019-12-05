#!/bin/bash

#have we got a paramater ?
if [ -z ${1+x} ]
then
	num_cpu=10
else
	num_cpu=$1
fi

#not greater than 100
if [ $num_cpu -gt 100 ]
then
	num_cpu=100
fi

source funcs.sh

for ((cpu=$num_cpu-1; cpu>=0; cpu--))
do
	links=""
	for ((lcpu=0; lcpu<$num_cpu; lcpu++))
	do
		zero_pad $cpu
		c1=$zp
		zero_pad $lcpu
		c2=$zp
		add_link $c1 $c2
	done
	boot_cpu_tui $cpu "$links"
done
