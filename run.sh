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

source funcs.sh

for ((cpu=$num_cpu-1; cpu>=0; cpu--))
do
	links=""
	for ((lcpu=0; lcpu<$num_cpu; lcpu++))
	do
		c1=$cpu
		c2=$lcpu
		zero_pad $c1
		c1=$zp
		zero_pad $c2
		c2=$zp
		add_link $c1 $c2
	done
	boot_cpu $cpu "$links"
done
