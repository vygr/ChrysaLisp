#!/bin/bash

#have we got a paramater ?
if [ -z ${1+x} ]
then
	num_cpu=64
else
	num_cpu=$1
fi

#not greater than 1000
if [ $num_cpu -gt 1000 ]
then
	num_cpu=1000
fi

source funcs.sh

for ((cpu=$num_cpu-1; cpu>=0; cpu--))
do
	links=""

	lcpu=$(((cpu-1) / 2))
	zero_pad $cpu
	c1=$zp
	zero_pad $lcpu
	c2=$zp
	add_link $c1 $c2

	lcpu=$(((cpu*2)+1))
	if [ $lcpu -lt $num_cpu ]
	then
		zero_pad $cpu
		c1=$zp
		zero_pad $lcpu
		c2=$zp
		add_link $c1 $c2
	fi

	lcpu=$(((cpu*2)+2))
	if [ $lcpu -lt $num_cpu ]
	then
		zero_pad $cpu
		c1=$zp
		zero_pad $lcpu
		c2=$zp
		add_link $c1 $c2
	fi

	boot_cpu $cpu "$links"
done
