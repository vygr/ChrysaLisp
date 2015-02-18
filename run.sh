#!/bin/bash
./main -cpu 0 -l 0-1 -l 0-2 -l 0-3 &
./main -cpu 1 -l 1-2 -l 1-3 -l 0-1 &
./main -cpu 2 -l 2-3 -l 0-2 -l 1-2 &
./main -cpu 3 -l 0-3 -l 1-3 -l 2-3 &
