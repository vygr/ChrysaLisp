#!/bin/bash

#common functions
source funcs.sh

#process args defaults
main 8 8 $@

if [ $num_cpu -ne 0 ]
then
	for ((cpu_y=$num_cpu-1; cpu_y>=0; cpu_y--))
	do
		for ((cpu_x=$num_cpu-1; cpu_x>=0; cpu_x--))
		do
			cpu=$(($cpu_y*$num_cpu + $cpu_x))
			links=""
			for ((lcpu_y=$cpu_y-1; lcpu_y<=$cpu_y+1; lcpu_y++))
			do
				wrap $lcpu_y
				add_link $cpu $(($wp*$num_cpu + $cpu_x))
			done
			for ((lcpu_x=$cpu_x-1; lcpu_x<=$cpu_x+1; lcpu_x++))
			do
				wrap $lcpu_x
				add_link $cpu $(($cpu_y*$num_cpu + $wp))
			done
			boot_cpu_gui $cpu $emu "$links"
		done
	done
fi