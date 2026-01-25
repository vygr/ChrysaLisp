#!/bin/bash

#common functions
source funcs.sh

#process args defaults
main 10 32 $@

if [ $num_cpu -ne 0 ]
then
	#save terminal state
	stty_save=$(stty -g)
	#set raw mode
	stty -icanon -echo min 1 time 0
	
	for ((cpu=$num_cpu-1; cpu>=0; cpu--))
	do
		links=""
		for ((lcpu=0; lcpu<$num_cpu; lcpu++))
		do
			add_link $cpu $lcpu
		done
		boot_cpu_tui $cpu "$links"
	done
	
	#restore terminal state
	stty $stty_save
fi
