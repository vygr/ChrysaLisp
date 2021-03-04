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
