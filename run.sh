#!/bin/bash

#common functions
source funcs.sh

#process args
main $@

if [ "$help" == "" ]
then
	boot_gui
fi
