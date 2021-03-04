#!/bin/bash

#common functions
source funcs.sh

#process args defaults
main 64 64 $@

if [ $num_cpu -ne 0 ]
then
	for ((cpu=$num_cpu-1; cpu>=0; cpu--))
	do
		links=""
		for ((lcpu=$cpu-1; lcpu<=$cpu+1; lcpu++))
		do
			wrap $lcpu
			add_link $cpu $wp
		done
		boot_cpu_gui $cpu $emu "$links"
	done
fi