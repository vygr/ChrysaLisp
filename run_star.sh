#!/bin/bash

#have we got a paramater ?
if [ -z ${1+x} ]
then
	num_cpu=64
else
	num_cpu=$1
fi

#not greater then 1000
if [ $num_cpu -gt 1000 ]
then
	num_cpu=1000
fi

source funcs.sh

links_to_0=""
for ((cpu=$num_cpu-1; cpu>0; cpu--))
do
	links=""
	zero_pad $cpu
	c1=$zp
	zero_pad 0
	c2=$zp
	add_link $c1 $c2
	links_to_0+="$links"
	boot_cpu $cpu "$links"
done
boot_cpu $cpu "$links_to_0"
