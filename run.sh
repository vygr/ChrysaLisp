#!/bin/bash

#process args defaults
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
		zero_pad $cpu
		c1=$zp
		zero_pad $lcpu
		c2=$zp
		add_link $c1 $c2
	done
	boot_cpu_gui $cpu $emu "$links"
done
