#!/bin/bash

#process args defaults
base_cpu=0
num_cpu=10
emu=""
for var in "$@"
do
	if [ $var == "-e" ]
	then
		emu=$var
	else
		num_cpu=$var
	fi
done

#not greater than 16
if [ $num_cpu -gt 16 ]
then
	num_cpu=16
fi

source funcs.sh

for ((cpu=$num_cpu-1; cpu>=0; cpu--))
do
	links=""
	for ((lcpu=0; lcpu<$num_cpu; lcpu++))
	do
		add_link $cpu $lcpu
	done
	boot_cpu_tui $cpu $emu "$links"
done
