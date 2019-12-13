#!/bin/bash

./stop.sh
make clean
make -j
./run_mesh.sh
