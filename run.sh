#!/bin/bash

#common functions
source funcs.sh

#process args defaults
main 10 16 $@

if [ $num_cpu -ne 0 ]
then
	for ((cpu=$num_cpu-1; cpu>=0; cpu--))
	do
		links=""
		for ((lcpu=0; lcpu<$num_cpu; lcpu++))
		do
			add_link $cpu $lcpu
		done
		boot_cpu_gui $cpu $emu "$links"
	done
fi