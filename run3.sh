#!/bin/bash
./main -cpu 2 -l /01-02 &
./main -cpu 1 -l /01-02 -l /00-01 &
./gui -cpu 0 -l /00-01 -run tests/test1 &
