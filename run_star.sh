#!/bin/bash

#common functions
source funcs.sh

#process args defaults
main 64 64 $@

if [ $num_cpu -ne 0 ]
then
	links_to_0=""
	for ((cpu=$num_cpu-1; cpu>0; cpu--))
	do
		links=""
		add_link $cpu 0
		links_to_0+="$links"
		boot_cpu_gui $cpu $emu "$links"
	done
	boot_cpu_gui $cpu $emu "$links_to_0"
fi