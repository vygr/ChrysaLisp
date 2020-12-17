#!/bin/bash

#have we got a paramater ?
if [ -z ${1+x} ]
then
	num_cpu=64
else
	num_cpu=$1
fi

#not greater than 64
if [ $num_cpu -gt 64 ]
then
	num_cpu=64
fi

source funcs.sh

for ((cpu=$num_cpu-1; cpu>=0; cpu--))
do
	links=""
	for ((lcpu=$cpu-1; lcpu<=$cpu+1; lcpu++))
	do
		wrap $lcpu
		zero_pad $cpu
		c1=$zp
		zero_pad $wp
		c2=$zp
		add_link $c1 $c2
	done
	boot_cpu_gui $cpu "$links"
done
