#!/bin/bash
./main -cpu 7 -l /00-07 -l /06-07 &
./main -cpu 6 -l /06-07 -l /05-06 &
./main -cpu 5 -l /05-06 -l /04-05 &
./main -cpu 4 -l /04-05 -l /03-04 &
./main -cpu 3 -l /03-04 -l /02-03 &
./main -cpu 2 -l /02-03 -l /01-02 &
./main -cpu 1 -l /01-02 -l /00-01 &
./main -cpu 0 -l /00-01 -l /00-07 -run tests/test1 &
