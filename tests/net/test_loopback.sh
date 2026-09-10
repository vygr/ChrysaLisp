#!/bin/bash
set -e

# ChrysaLisp network link loopback test runner
# Cleans up background processes on exit
cleanup() {
	./stop.sh > /dev/null 2>&1 || true
}
trap cleanup EXIT

# 1. Ensure clean slate
./stop.sh > /dev/null 2>&1 || true

# 2. Start server in background
./run_tui.sh -s tests/net/srv_loopback.lisp > tests/net/server.log 2>&1 &
SRV_PID=$!

# 3. Wait for port 4567 to open
for i in {1..30}; do
	if lsof -i :4567 > /dev/null 2>&1; then
		break
	fi
	sleep 0.1
done

# 4. Run client
./run_tui.sh -b 10 -s tests/net/cli_loopback.lisp
