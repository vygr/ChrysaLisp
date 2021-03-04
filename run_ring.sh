#!/bin/bash

#process args defaults
base_cpu=0
num_cpu=64
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

#not greater than 64
if [ $num_cpu -gt 64 ]
then
	num_cpu=64
fi

source funcs.sh

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
