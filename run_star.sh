#!/bin/bash

#process args defaults
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

links_to_0=""
for ((cpu=$num_cpu-1; cpu>0; cpu--))
do
	links=""
	zero_pad $cpu
	c1=$zp
	zero_pad 0
	c2=$zp
	add_link $c1 $c2
	links_to_0+="$links"
	boot_cpu_gui $cpu $emu "$links"
done
boot_cpu_gui $cpu $emu "$links_to_0"
