#!/bin/bash

#have we got a paramater ?
if [ -z ${1+x} ]
then
	num_cpu=8
else
	num_cpu=$1
fi

#not greater then 1000
if [ $num_cpu -gt 100 ]
then
	num_cpu=100
fi

source funcs.sh

for ((cpu_y=$num_cpu-1; cpu_y>=0; cpu_y--))
do
	for ((cpu_x=$num_cpu-1; cpu_x>=0; cpu_x--))
	do
		cpu=$(($cpu_y*$num_cpu + $cpu_x))
		links=""
		for ((lcpu_y=$cpu_y-1; lcpu_y<=$cpu_y+1; lcpu_y++))
		do
			wrap $lcpu_y

			c1=$cpu
			c2=$(($wp*$num_cpu + $cpu_x))

			zero_pad $c1
			c1=$zp
			zero_pad $c2
			c2=$zp
			add_link $c1 $c2
		done
		for ((lcpu_x=$cpu_x-1; lcpu_x<=$cpu_x+1; lcpu_x++))
		do
			wrap $lcpu_x

			c1=$cpu
			c2=$(($cpu_y*$num_cpu + $wp))

			zero_pad $c1
			c1=$zp
			zero_pad $c2
			c2=$zp
			add_link $c1 $c2
		done
		boot_cpu $cpu "$links"
	done
done
