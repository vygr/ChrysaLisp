#!/bin/bash
./main -cpu 3 -l /00-03 -l /01-03 -l /02-03 &
./main -cpu 2 -l /02-03 -l /00-02 -l /01-02 &
./main -cpu 1 -l /01-02 -l /01-03 -l /00-01 &
./main -cpu 0 -l /00-01 -l /00-02 -l /00-03 -run tests/test1 &
