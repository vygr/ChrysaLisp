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

		lcpu=$(((cpu-1) / 2))
		add_link $cpu $lcpu

		lcpu=$(((cpu*2)+1))
		if [ $lcpu -lt $num_cpu ]
		then
			add_link $cpu $lcpu
		fi

		lcpu=$(((cpu*2)+2))
		if [ $lcpu -lt $num_cpu ]
		then
			add_link $cpu $lcpu
		fi

		boot_cpu_gui $cpu $emu "$links"
	done
fi